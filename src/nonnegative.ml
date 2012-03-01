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

(* This module is a contribution of Yuto Takei *)

open Sig
open Blocks

module type WEIGHT = sig
  type label
  type t
  val weight : label -> t
  val compare : t -> t -> int
  val add : t -> t -> t
  val zero : t
end

(****
module NonNegative = struct
  module Persistent
    (G: Sig.P) (* is it better to give V and E to construct graph? *)
    (W: WEIGHT with type label = G.E.label) : Sig.P = struct

    module H = Map.Make(G.V)

      (* Graph itself, source list, distance table *)
    type t = G.t * G.V.t list * G.E.t H.t H.t

    exception NegativeCycle of G.E.t list

    let add_vertex (g:t) (v:G.V.t) : t =
      (* Check first not to duplicate add the vertext to the graph *)
      let (g, src, dist) = g in

      (* Simply add a vertex to the original one *)
      let g_ = G.add_vertex g v in
      if g = g_ then

	(* If the graph is returned as is, v is already in the graph *)
	(* TODO: Check si c'est vrai? *)
	(g, src, dist)

      else begin
	(* The new vertex will immediately be added to the new list *)
	let src = v :: src in

	(* Copy the given hashtable so that the data itself can be modified *)
	let vDist = H.create 97 in
	H.add dist v 0;
	H.add dist v vDist;

	(* Return in the same structure *)
	(g, src, dist)
      end

    let propagate (g:t) (v:G.V.t) (q:Queue.t) (target:G.V.t list) : t =
      if Queue.is_empty q then g
      else begin
	let (g, src, dist) = g in

	let desc = G.succ g v in
	


	(* Add descendent nodes to the queue for distance propagation *)
        (* TODO: Optimizable *)
        List.iter (Queue.push q) desc


      end

    let add_edge_e (g:t) (e:G.V.t) : t =
      let (g, src, dist) = g in

      (* Check the edge is already existed in the graph or not *)

      let v1 = E.src e in
      let v2 = E.dst e in
      
      let dv1 = H.copy (H.find dist v1) in
      let dv2 = H.copy (H.find dist v2) in

      (* We need to check whether v2 should be kept in the source list or not.
	 That is, if there maybe a cycle with v1, the distance from v1 should be
	 still maintained. Otherwise, simply ignore the distance from v2 *)
      let keepV2Src = (H.length dv1 == 1) && (H.mem dv1 v2) in

      (* To reduce the amount of codes, we just start propagation from v1. Of
	 course, this can be optimized, with starting from v2. But it may duplicate
	 the same code in multiple places in the file *)
      let q = Queue.create () in
      Queue.add q v1
      propagate (g, src, dist) q 
	
      

    let add_edge (g:t) (v1:G.V.t) (v2:G.V.t) : t =
      add_edge_e (* new edge, and call add_edge_e *)


    let remove_vertex _ =
      assert false (* TODO *)


  end
end
****)
