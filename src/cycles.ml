
type weight =
  | Normal of int
  | Obligatory of int

module Fashwo
 (GB : sig
         include Builder.S
         val weight : G.edge -> weight
       end)
=
struct
  module G = GB.G

  exception Stuck of G.vertex list

  module IM = Map.Make (struct type t = int let compare = Stdlib.compare end)
  module VM = Map.Make (G.V)
  module VS = Set.Make (G.V)

  (* The algorithm of Eades, Lin, and Smyth (ELS 1993) works by "scheduling"
     vertexes onto two lists called s1 and s2. At each iteration a vertex is
     chosen, scheduled, and removed from the graph. Arcs from a newly scheduled
     node toward nodes already in s1 are classified as "leftward"; they are
     included in the generated feedback arc set. "Rightward" arcs, to vertexes
     in s2 or that have not yet been scheduled, are not included in the
     feedback arc set. The algorithm tries to maximize the number of rightward
     arcs and thereby minimize the number of leftward ones. Source vertexes,
     those with no incoming arcs in the current graph (i.e., because all its
     predecssors have already been scheduled), are appended directly onto s1
     and do not induce any feedback arcs. Sink vertexes are consed directly
     onto s2 and do not induce any feedback arcs. Otherwise, the algorithm
     chooses a vertex to maximize the difference between the number of
     outgoing arcs and the number of incoming ones: the (remaining) incoming
     arcs must be included in the feedback arc set. The difference between the
     number of rightward arcs (no cost) and the number of leftward arcs
     (feedback arcs) is called "delta". The algorithm is implemented
     efficiently by using a data structure to group unscheduled vertexes
     according to their delta value. When more than one vertex has the maximum
     delta value, the original algorithm makes an arbitrary choice. The
     algorithm of Eades and Lin (EL 1995) makes the choice using a heuristic
     that maximizes the difference between incoming arcs and outgoing ones in
     the vertexes that remain at the end of the iteration as such vertexes are
     the most "unbalanced" and thus less likely to contribute to the feedback
     arc set in future iterations. The EL 1995 algorithm includes a further
     refinement to ignore chains of vertexes when looking for unbalanced ones,
     since such chains do not contribute feedback arcs.

     Since we just want to produce a list of feedback arcs, we don't bother
     tracking order in s1, and we only track s2 to properly handle the
     preprocessing optimization that removes two cycles. We maintain lists of
     source and sink vertexes (scheduled but not yet removed from the graph)
     and a map from delta values to sets of vertexes. As the delta value map
     caches the state of the graph, it must be updated when the a vertex is
     scheduled and removed from the graph. Additionally, we remember which two
     cycles were removed during preprocessing and ensure that one of their
     arcs is included in the feedback arc set, depending on whichever of the
     two interlinked vertexes is scheduled first. *)

  type t = {
    s1         : VS.t;             (* vertexes placed "at left" *)
    s2         : VS.t;             (* vertexes placed "at right";
                                      only needed to optimize for two_cycles *)
    sources    : VS.t;             (* vertexes with no incoming arcs *)
    sinks      : VS.t;             (* vertexes with no outgoing arcs *)
    delta_bins : VS.t IM.t;        (* group vertexes by delta value *)
    vertex_bin : int VM.t;         (* map each vertex to its bin *)
    two_cycles : G.edge list VM.t; (* edges for 2-cycles *)
    fas        : G.edge list;      (* current feedback arc set *)
  }

  let empty = {
      s1 = VS.empty;
      s2 = VS.empty;
      sources = VS.empty;
      sinks = VS.empty;
      delta_bins = IM.empty;
      vertex_bin = VM.empty;
      two_cycles = VM.empty;
      fas = [];
    }

  let add_to_bin delta v ({ delta_bins; vertex_bin; _ } as st) =
    { st with delta_bins =
                IM.update delta (function None -> Some (VS.singleton v)
                                        | Some vs -> Some (VS.add v vs))
                  delta_bins;
                vertex_bin = VM.add v delta vertex_bin }

  let remove_from_bin v ({ delta_bins; vertex_bin; _ } as st) =
    match VM.find_opt v vertex_bin with
    | None -> st
    | Some delta ->
        { st with delta_bins =
                    IM.update delta (function None -> None
                                            | Some vs -> Some (VS.remove v vs))
                      delta_bins;
                  vertex_bin = VM.remove v vertex_bin }

  (* Calculate the sums of incoming and outgoing edge weights, ignoring
     obligatory arcs; they must be respected so their weight is irrelevant. *)
  let weights g v =
    let add_pweight e (s, b) =
      match GB.weight e with Obligatory _ -> (s, true) | Normal w -> (s + w, b)
    in
    let add_sweight e s =
      match GB.weight e with Obligatory w -> s + w | Normal w -> s + w
    in
    let inw, blocked = G.fold_pred_e add_pweight g v (0, false) in
    let outw = G.fold_succ_e add_sweight g v 0 in
    blocked, inw, outw

  let add_vertex g v delta ({ sources; sinks; _ } as st) =
    let ind, outd = G.in_degree g v, G.out_degree g v in
    if ind = 0 then { st with sources = VS.add v sources }
    else if outd = 0 then { st with sinks = VS.add v sinks }
    else add_to_bin delta v st

  (* Initialize the state for a given vertex. *)
  let init_vertex g v st =
    let blocked, inw, outw = weights g v in
    if blocked then st else add_vertex g v (outw - inw) st

  let init g = G.fold_vertex (init_vertex g) g empty

  (* Move v from the bin for delta to sources, sinks, or another bin. *)
  let shift_bins g v delta' st0 = add_vertex g v delta' (remove_from_bin v st0)

  (* Before removing v from the graph, update the state of its sucessors. *)
  let update_removed_succ g' e st =
    let v = G.E.dst e in
    let still_blocked, inw', outw' = weights g' v in
    if still_blocked then st else shift_bins g' v (outw' - inw') st

  (* Before removing v from the graph, update the state of its predecessors. *)
  let update_removed_pred g' e ({ sinks; _ } as st) =
    let v = G.E.src e in
    let blocked, inw', outw' = weights g' v in
    match GB.weight e with
    | Obligatory _ ->
        if blocked || outw' > 0 then st
        else (* not blocked && outw' = 0 *)
        { (remove_from_bin v st) with sinks = VS.add v sinks }
    | Normal _ ->
        if blocked then st else shift_bins g' v (outw' - inw') st

  (* Remove a vertex from the graph and update the data structures for its
     succesors and predecessors. *)
  let remove_vertex g v st =
    let g' = GB.remove_vertex g v in
    (g', G.fold_succ_e (update_removed_succ g') g v st
         |> G.fold_pred_e (update_removed_pred g') g v)

  (* The original article proposes preprocessing the graph to condense long
     chains of vertexes. This works together with the heuristic for generating
     unbalanced vertexes, since the intermediate nodes on the chain do not
     contribute any leftward arcs (when the last vertex is removed, they
     become a sequence of sinks). Using such a preprocessing step with
     weighted edges risks removing good feedback arcs, i.e., those with a big
     difference between outgoing and incoming weights. That is why here we
     use on-the-fly condensation, even if there is a risk of recomputing the
     same result several times. *)
  let rec condense w g v =
    if G.out_degree g v = 1 then
      match G.pred g v with
      | [u] when not (G.V.equal u w) -> condense w g u
      | _ -> v
    else v

  (* Find the vertex v that has the most "unbalanced" predecessor u. Most
     unbalanced means the biggest difference between the input weights and
     output weights. Skip any vertex with an incoming obligatory arc. *)
  let takemax g v imax =
    let check_edge e max = (* check u -> v *)
      let u_blocked, u_inw, u_outw =
        weights g (condense (G.E.dst e) g (G.E.src e)) in
      let u_w = u_inw - u_outw in
      match max with
      | Some (None, _)
      | None -> Some ((if u_blocked then None else Some u_w), v)
      | Some (Some x_w, _) when u_w > x_w -> Some (Some u_w, v)
      | _ -> max
    in
    G.fold_pred_e check_edge g v imax

  (* Look for the vertex with the highest delta value that is not the target
     of an obligatory arc. Use the "unbalanced" heuristic impllemented in
     [takemax] to discriminate between competing possibilities. If a vertex
     is found, remove it from the returned delta bins. *)
(*
  let max_from_deltas g ({ delta_bins; _ } as st) =
    let rec f = function
      | Seq.Nil -> None
      | Seq.Cons ((_, dbin), tl) ->
          (match VS.fold (takemax g) dbin None with
           | None -> f (tl ())
           | Some (_, v) -> Some (v, remove_from_bin v st))
    in
    f (IM.to_rev_seq delta_bins ())
*)
  let max_from_deltas g ({ delta_bins; _ } as st) =
    let rec f im =
      if IM.is_empty im then
        None
      else
        let k, dbin = IM.max_binding im in
        (match VS.fold (takemax g) dbin None with
           | None -> f (IM.remove k im)
           | Some (_, v) -> Some (v, remove_from_bin v st))
    in
    f delta_bins

  (* Include any leftward arcs due to the two-cycles that were removed by
     preprocessing. *)
  let add_from_two_cycles s1 s2 two_cycles v fas =
    let bf es b = if G.V.equal (G.E.dst b) v then b::es else es in
    let f es e =
      let w = G.E.dst e in
      if VS.mem w s1 then e::es
      else if VS.mem w s2 then
        (* the two-cycle partner has already been scheduled as sink, so
           the feedback edges come from it. *)
        match VM.find_opt w two_cycles with
        | None -> es
        | Some bs -> List.fold_left bf es bs
      else es in
    match VM.find_opt v two_cycles with
    | None -> fas
    | Some es -> List.fold_left f fas es

  (* Shift a given vertex onto s1, and add any leftward arcs to the feedback
     arc set. *)
  let schedule_vertex g (v, ({ s1; s2; fas; two_cycles; _ } as st)) =
    let add_to_fas e es = if VS.mem (G.E.src e) s1 then es else e::es in
    (v, { st with s1 = VS.add v s1;
                  fas = G.fold_pred_e add_to_fas g v fas
                          |> add_from_two_cycles s1 s2 two_cycles v })

  (* Take the next available vertex from, in order, sources, sinks, or the
     highset possible delta bin. *)
  let choose_vertex g ({ s1; s2; sources; sinks; two_cycles; fas; _ } as st0) =
    match VS.choose_opt sources with
    | Some v ->
        Some (v, { st0 with sources = VS.remove v sources;
                            sinks = VS.remove v sinks;
                            s1 = VS.add v s1;
                            fas = add_from_two_cycles s1 s2 two_cycles v fas })
    | None ->
        (match VS.choose_opt sinks with
         | Some v ->
             Some (v, { st0 with sinks = VS.remove v sinks;
                                 s2 = VS.add v s2;
                                 fas = add_from_two_cycles s1 s2 two_cycles v fas })
         | None -> Option.map (schedule_vertex g) (max_from_deltas g st0))

  let add_two_cycle_edge two_cycles e =
    VM.update (G.E.src e) (function None -> Some [e]
                                  | Some es -> Some (e :: es)) two_cycles

  let same_weight w e =
    match GB.weight e with
    | Obligatory _ -> false
    | Normal w' -> w' = w

  (* For every pair of distinct vertexes A and B linked to each other by
     edges A -ab-> B and B -ba-> A with the same weight, update the mapping
     by linking A to ab, and B to ba, and remove the edges from the graph.
     When A is scheduled, if B is already in s1 then the edge ab is a
     feedback arc, and similarly for B and ba. The principle is that there
     will be a feedback arc regardless of whether A is "scheduled" before B or
     vice versa, therefore such cycles should not constrain vertex choices. *)
  let remove_two_cycles g0 =
    let f e ((g, cycles) as unchanged) =
      match GB.weight e with
      | Obligatory _ -> unchanged
      | Normal w ->
          if List.length (G.find_all_edges g0 (G.E.src e) (G.E.dst e)) > 1
          (* invalid for graphs like: { A -1-> B, A -2-> B, B -3-> A *)
          then raise Exit
          else
            let back_edges =
              G.find_all_edges g0 (G.E.dst e) (G.E.src e)
              |> List.filter (same_weight w)
            in
            if back_edges = [] then unchanged
            else (GB.remove_edge_e g e,
                  List.fold_left add_two_cycle_edge cycles back_edges)
    in
    try
      G.fold_edges_e f g0 (g0, VM.empty)
    with Exit -> (g0, VM.empty)

  (* All self loops must be broken, so just add them straight into the
     feedback arc set. *)
  let remove_self_loops g0 =
    let f v (g, fas) =
      let self_loops = G.find_all_edges g0 v v in
      (List.fold_left GB.remove_edge_e g self_loops,
       List.rev_append self_loops fas)
    in
    G.fold_vertex f g0 (g0, [])

  (* Remove any arcs between strongly connected components. There can be no
     cycles between distinct sccs by definition. *)
  module C = Components.Make(G)
  module Emap = Gmap.Edge(G)(struct include GB.G include GB end)

  let disconnect_sccs g =
    let nsccs, fscc = C.scc g in
    let in_same_scc e =
      if fscc (G.E.src e) = fscc (G.E.dst e) then Some e else None
    in
    if nsccs < 2 then g
    else Emap.filter_map in_same_scc g

  let feedback_arc_set g0 =
    let rec loop (g, st) =
      match choose_vertex g st with
      | Some (v, st') when G.mem_vertex g v -> loop (remove_vertex g v st')
      | Some (_, st') -> loop (g, st')
      | None ->
          let remaining = IM.fold (Fun.const VS.union) st.delta_bins VS.empty in
          if VS.is_empty remaining then st.fas
          else raise (Stuck (VS.elements remaining))
    in
    let g1 = disconnect_sccs g0 in
    let g2, fas = remove_self_loops g1 in
    let g3, two_cycles = remove_two_cycles g2 in
    loop (g3, { (init g3) with fas; two_cycles })

end

