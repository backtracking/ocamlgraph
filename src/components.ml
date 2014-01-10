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

open Util

module type G = sig
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
end

module Make(G: G) = struct

  module H = Hashtbl.Make(G.V)

  let scc g =
    let root = H.create 997 in
    let hashcomp = H.create 997 in
    let stack = ref [] in
    let numdfs = ref 0 in
    let numcomp = ref 0 in
    let rec pop x = function
      | (y, w) :: l when y > x ->
	  H.add hashcomp w !numcomp;
	  pop x l
      | l -> l
    in
    let rec visit v =
      if not (H.mem root v) then begin
	let n = incr numdfs; !numdfs in
	H.add root v n;
	G.iter_succ
	  (fun w ->
	    visit w;
	    if not (H.mem hashcomp w) then
	      H.replace root v (min (H.find root v) (H.find root w)))
	  g v;
	if H.find root v = n then begin
	  H.add hashcomp v !numcomp;
	  let s = pop n !stack in
	  stack:= s;
	  incr numcomp
	end else
          stack := (n,v) :: !stack;
      end
    in
    G.iter_vertex visit g;
    !numcomp, (fun v -> H.find hashcomp v)

  let scc_array g =
    let n,f = scc g in
    let t = Array.make n [] in
    G.iter_vertex (fun v -> let i = f v in t.(i) <- v :: t.(i)) g;
    t

  let scc_list g =
    let a = scc_array g in
    Array.fold_right (fun l acc -> l :: acc) a []

end
