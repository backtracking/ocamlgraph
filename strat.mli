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

(* $Id:$ *)

(** Strategies *)

(** Signature for graphs *)
module type G = sig
  type t
  module V : Sig.ORDERED_TYPE
  type vertex = V.t
  val mem_vertex : t -> vertex -> bool
  val succ : t -> vertex -> vertex list
  val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
end

(** Signature for graph add-ons: an initial vertex, final vertices
   and membership of vertices to either true or false,
   i.e. first or second player *)
module type PLAYER = sig

  type t
  type vertex

  val get_initial : t -> vertex
  val is_final : t -> vertex -> bool

  val turn : t -> vertex -> bool

end

(** Signature for strategies: for a given state, the strategy tells
   which state to go to *)
module type STRAT = sig

  type t
  type vertex

  val empty : t
  val add : t -> vertex -> vertex -> t

  val next : t -> vertex -> vertex
    (* Raises [Invalid_argument] if vertex's image is not defined *)

end

(** Implements strategy algorithms on graphs *)
module Algo (G : G) (P : PLAYER with type vertex = G.vertex)
  (S : STRAT with type vertex = G.vertex) :
sig

  (** [coherent_player g p] returns [true] iff
     the completion [p] is coherent w.r.t.
     the graph g *)
  val coherent_player : G.t -> P.t -> bool

  (** [coherent_strat g s] returns [true] iff
     the strategy [s] is coherent w.r.t.
     the graph [g] *)
  val coherent_strat : G.t -> S.t -> bool

  (** [game g p a b] returns [true] iff [a] wins in [g]
     given the completion [p] (i.e. the game
     goes through a final state). *)
  val game : G.t -> P.t -> S.t -> S.t -> bool

  (** [strategy g p s] returns [true] iff [s] wins in [g]
     given the completion [p], whatever strategy
     plays the other player. *)
  val strategy : G.t -> P.t -> S.t -> bool

  (** [strategyA g p] returns [true] iff there
      exists [a] winning stragegy for the true
      player. In this case, the winning
      strategy is provided. *)
  val strategyA : G.t -> P.t -> (bool * S.t)
end
