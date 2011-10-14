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

(* Copyright (c) 2010 - 2011 Technische Universitaet Muenchen
 * Markus W. Weissmann <markus.weissmann@in.tum.de>
 * All rights reserved. *)

(** Fixpoint computation implemented using the work list algorithm.
    This module makes writing data-flow analysis easy. *)

(** Minimal graph signature for work list algorithm *)
module type G = sig
  type t
  module V : Sig.COMPARABLE
  module E : sig
    type t
    type label
    val label : t -> label
    val dst : t -> V.t
    val src : t -> V.t
  end
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val succ_e : t -> V.t -> E.t list
  val pred_e : t -> V.t -> E.t list
  val succ : t -> V.t -> V.t list
  val pred : t -> V.t -> V.t list
end

type direction = Forward | Backward
(** Type of an analysis *)

module type Analysis = sig
  type data
    (** information stored at each vertex *)
  type label
    (** the label of edges of the underlying graph *)
  type vertex
    (** type of a vertex of the underlying graph *)
  type cfg
    (** type of the underlying graph *)
  val direction : direction
    (** the direction of the analysis *)
  val join : data -> data -> data
    (** operation how to join data when paths meet *)
  val equal : data -> data -> bool
    (** predicate to determine the fixpoint *)
  val analyze : label -> data -> data
    (** the actual analysis of one label; provided the label and the incoming
        data, it needs to compute the outgoing data *)
  end

module Make
  (G : G)
  (A : Analysis with type cfg = G.t with type label = G.E.label
                with type vertex = G.V.t) :
sig
  val analyze : (G.V.t -> A.data) -> A.cfg -> G.V.t -> A.data
  (** [analyze f g] computes the fixpoint on the given graph using the
      work list algorithm. Beware that a misconstructed Analysis will
      not terminate! [f] is used to create the initial analysis
      data. The function returned is a map to see what data was computed
      for which node. *)
end
