(*
 * Graph: generic graph library
 * Copyright (C) 2004
 * Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 *)

(* $Id: components.mli,v 1.12 2004-10-22 14:42:06 signoles Exp $ *)

(** Strongly connected components *)

open Util

(** Minimal graph signature for [scc] *)
module type G = sig
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
end

module Make (G: G) : sig

  val scc : G.t -> (G.V.t -> int)
      (** [scc g] computes the strongly connected components of [g].
	The result is a function [f] such that [f u = f v] if and only if
	[u] and [v] are in the same component.
	Beware: to be used efficiently, [scc] must be applied to a single
	argument (the graph). *)

  val scc_list : G.t -> G.V.t list list
    (** [scc_list] computes the strongly connected components of [g].
      The result is a partition of the set of the vertices of [g]. *)

end
