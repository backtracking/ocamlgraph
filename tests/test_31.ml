
(* Issue #31

   This would fail in the current state of OCamlGraph,
   as map_vertex requires a supplied function that is injective.
*)

module String = struct include String let hash = Hashtbl.hash end
module G = Graph.Persistent.Digraph.ConcreteBidirectional(String)

(* Make a persistent graph where:
     A -> B
     B -> C
     C -> B *)
let g = List.fold_left (fun g (x, y) ->
            G.add_edge g x y) G.empty [("A", "B"); ("B", "C"); ("C", "B")]

(* Contract the SCC *)
module C = Graph.Components.Make(G)

let contract g =
  let names = Array.map (String.concat ",") (C.scc_array g) in
  let (_, numbering) = C.scc g in
  Array.fold_left (fun g x -> G.remove_edge g x x)
    (G.map_vertex (fun x -> names.(numbering x)) g)
    names

let g' = contract g
(* this is now "A" -> "C,B" *)

let () =
  assert (G.in_degree g' "C,B" = 1)
