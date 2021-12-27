(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

module type G = sig
  type t
  val is_directed : bool
  module V : Sig.COMPARABLE
  module E : Sig.EDGE with type vertex = V.t
  val iter_edges_e : (E.t -> unit) -> t -> unit
end

module Make(G: G) = struct

  open G

  let rev e =
    E.create (E.dst e) (E.label e) (E.src e)

  module H = Hashtbl.Make(V)

  let setup g =
    let nbe = ref 0 in
    let out = H.create 16 in
    let add h x y e =
      let s = try H.find h x
              with Not_found -> let s = H.create 4 in H.add h x s; s in
      if H.mem s y then invalid_arg "Eulerian.path (multigraphs not allowed)";
      H.add s y e in
    let add e =
      incr nbe;
      let x = E.src e and y = E.dst e in
      add out x y e;
      if not is_directed && not (V.equal x y) then add out y x (rev e) in
    iter_edges_e add g;
    !nbe, out

  exception Found of V.t
  let any h =
    try H.iter (fun v _ -> raise (Found v)) h; assert false
    with Found v -> v, H.find h v

  (** in order to achieve optimal complexity, paths are built as
     doubly-linked lists, so that we can merge two cycles with a
     common vertex in constant time *)
  type dll = { mutable prev: dll; edge: E.t; mutable next: dll }

  let remove_edge out e =
    (* Format.eprintf "remove_edge %a@." print_edge e; *)
    let remove h x y =
      let s = H.find h x in
      assert (H.mem s y);
      H.remove s y;
      if H.length s = 0 then H.remove h x in
    let v = E.src e and w = E.dst e in
    remove out v w

  let self e = V.equal (E.src e) (E.dst e)

  let remove_edge edges e =
    remove_edge edges e;
    if not is_directed && not (self e) then remove_edge edges (rev e)

  let any_out_edge out v =
    assert (H.mem out v);
    let s = H.find out v in
    assert (H.length s > 0);
    let _, e = any s in
    remove_edge out e;
    e

  (** builds an arbitrary cycle from [start] *)
  let round_trip edges start =
    let e = any_out_edge edges start in
    let rec path = { prev = path; edge = e; next = path } in
    let rec tour e =
      let v = E.dst e.edge in
      if V.equal v start then (
        path.prev <- e;
        path
      ) else (
        let e' = { prev = e; edge = any_out_edge edges v; next = path } in
        e.next <- e';
        tour e'
      ) in
    tour path

  let connect e e' =
    e.next <- e';
    e'.prev <- e

  (** builds an Eulerian cycle from [v] *)
  let eulerian_cycle out start =
    (* Format.eprintf "eulerian_cycle from start=%a@." print_vertex start; *)
    let todos = H.create 8 in (* vertex on cycle with out edges -> cycle edge *)
    let todo e =
      let v = E.src e.edge in
      if H.mem out v then H.replace todos v e else H.remove todos v in
    let rec update start e =
      todo e;
      if not (V.equal (E.dst e.edge) start) then update start e.next in
    let path = round_trip out start in
    update start path;
    (* H.iter (fun v s -> eprintf "    out %a = %d@." print_vertex v (H.length s)) out;
     * eprintf "  %d todos@." (H.length todos); *)
    while H.length todos > 0 do
      let v, e = any todos in
      (* Format.eprintf "  add cycle from %a@." print_vertex v; *)
      H.remove todos v;
      assert (H.mem out v);
      let e' = round_trip out v in
      update v e';
      (* H.iter (fun v s -> eprintf "    out %a = %d@." print_vertex v (H.length s)) out;
       * eprintf "  %d todos@." (H.length todos); *)
      let p = e.prev in
      assert (p.next == e);
      let p' = e'.prev in
      assert (p'.next == e');
      connect p e';
      connect p' e;
    done;
    path

  let list_of path =
    let rec convert acc e =
      if e == path then List.rev acc else convert (e.edge :: acc) e.next in
    convert [path.edge] path.next

  let mem_edge out x y =
    try H.mem (H.find out x) y with Not_found -> false

  let out_degree out x =
    try H.length (H.find out x) with Not_found -> 0

  let undirected g =
    let nbe, out = setup g in
    let odds = H.create 2 in
    let check v s =
      let d = H.length s in
      let d = if H.mem s v then d - 1 else d in
      if d mod 2 = 1 then H.add odds v () in
    H.iter check out;
    let n = H.length odds in
    if n <> 0 && n <> 2 then invalid_arg "Eulerian.path (bad degrees)";
    let cycle = n = 0 in
    let path =
      if cycle then
        if nbe = 0 then []
        else let v, _ = any out in list_of (eulerian_cycle out v)
      else (
        (* we have two vertices x and y with odd degrees *)
        let x, _ = any odds in
        H.remove odds x;
        let y, _ = any odds in
        if mem_edge out x y then (
          (* there is an edge x--y => it connects two Eulerian cycles *)
          let xy = H.find (H.find out x) y in
          remove_edge out xy;
          match out_degree out x, out_degree out y with
          | 0, 0 -> [xy]
          | _, 0 -> rev xy :: list_of (eulerian_cycle out x)
          | 0, _ -> xy :: list_of (eulerian_cycle out y)
          | _ ->
              (* a bit of a pity to use list concatenation, but this
                 does not change the complexity *)
              list_of (eulerian_cycle out x) @
                xy :: list_of (eulerian_cycle out y)
        ) else (
          (* no edge x--y => add one, build a cycle, then remove it *)
          let dummy = E.label (snd (any (H.find out x))) in
          let xy = E.create x dummy y in
          H.add (H.find out x) y xy;
          H.add (H.find out y) x (rev xy);
          let p = eulerian_cycle out x in
          let rec find e =
            let v = E.src e.edge and w = E.dst e.edge in
            if V.equal v x && V.equal w y ||
               V.equal v y && V.equal w x then e else find e.next in
          let start = find p in
          List.tl (list_of start)
        )
      )
    in
    (* check that all edges have been consumed *)
    if H.length out > 0 then invalid_arg "Eulerian.path (not connected)";
    path, cycle

  let directed _g =
    invalid_arg "Eulerian.path (directed graphs not yet supported)"

  let path g =
    if is_directed then directed g else undirected g

  let cycle g =
    let p, c = path g in
    if not c then invalid_arg "Eulerian.cycle";
    p

end
