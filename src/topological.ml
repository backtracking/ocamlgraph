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
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val in_degree : t -> V.t -> int
end

module type Q = sig
  type elt
  type q
  val create: unit -> q
  val push: elt -> q -> unit
  val pop: q -> elt
  val is_empty: q -> bool
  val choose: old:(elt list * int) -> elt * int -> elt list * int 
end

module Build(G: G)(Q: Q with type elt = G.V.t) = struct

  module H = Hashtbl.Make(G.V)
  module C = Path.Check(G)

  let fold f g acc =
    let checker = C.create g in
    let degree = H.create 997 in
    let todo = Q.create () in
    let push x =
      H.remove degree x;
      Q.push x todo
    in
    let rec walk acc =
      if Q.is_empty todo then
        (* let's find any node of minimal degree *)
	let min, _ =
	  H.fold (fun v d old -> Q.choose ~old (v, d)) degree ([], max_int)
	in
	match min with
	| [] -> acc
	| [ v ] -> push v; walk acc
	| v :: l ->
	  (* in case of multiple cycles, choose one vertex in a cycle which
	     does not depend of any other. *)
	  let v =
	    List.fold_left
	      (fun acc v' -> if C.check_path checker acc v' then acc else v')
	      v
	      l
	  in
	  push v; walk acc
      else
	let v = Q.pop todo in
	let acc = f v acc in
	G.iter_succ
	  (fun x->
             try
               let d = H.find degree x in
	       if d = 1 then push x else H.replace degree x (d-1)
             with Not_found ->
	       (* [x] already visited *)
	       ())
	  g v;
	walk acc
    in
    G.iter_vertex
      (fun v ->
	 let d = G.in_degree g v in
	 if d = 0 then Q.push v todo
	 else H.add degree v d)
      g;
    walk acc

  let iter f g = fold (fun v () -> f v) g ()

end

let choose ~old (v, n) =
  let l, min = old in
  if n = min then v :: l, n 
  else if n < min then [ v ], n
  else old

module Make(G: G) = 
  Build
    (G)
    (struct 
      include Queue 
      type elt = G.V.t 
      type q = G.V.t t 
      let choose = choose
     end)

module Make_stable(G: G) =
  Build
    (G)
    (struct
      module S = Set.Make(G.V)
      type elt = G.V.t
      type q = S.t ref
      let create () = ref S.empty
      let push v s = s := S.add v !s
      let pop s =
        let r = S.min_elt !s in
        s := S.remove r !s;
        r
      let is_empty s = S.is_empty !s
      let choose ~old new_ = 
	let l, n = choose ~old new_ in
	List.sort G.V.compare l, n
     end)

(*
Local Variables:
compile-command: "make -C .."
End:
*)
