(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2022                                               *)
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

(** Algorithms related to cycles in directed graphs. *)

type weight =
  | Normal of int
    (** Weighted arc that can be included in the feedback set. The
        weight must be zero (not normally a good choice) or positive
        (1 may be a good choice). *)
  | Obligatory of int
    (** Obligatory arc that cannot be returned in the feedback set.
        Set the weight to zero to completely ignore obligatory arcs
        when choosing which vertex to schedule. Set it to a positive
        value (1 may be a good choice) to adjust the preference for
        choosing vertexes that may "unblock" other vertexes by
        removing their incoming obligatory arcs. *)

(** Adaptation of the FASH algorithm of Eades and Lin (1995) to handle
    edge weights and obligatory arcs. The algorithm tries to minimize the
    total weight of the returned feedback arc set. Obligatory arcs are
    respected and never returned in the feedback arc set, an exception is
    raised if the obligatory arcs form a cycle. The adapted algorithm is
    hereby called FASHWO: “feedback arc set heuristic + weights and
    obligations”.

    For a graph G and any one of its feedback arc sets F, the graph G - F is
    obviously acyclic. If F is minimal, i.e., adding any of its edges to G - F
    would introduce a cycle, then reversing, rather than removing, the
    feedback arcs also gives an ayclic graph, [G - F + F^R]. In fact, Eades
    and Lin define the feedback arc set as "a set of arcs whose reversal makes
    G acyclic".

    @see <https://mathoverflow.net/a/234023/> David Epstein proof about reversed arcs *)
module Fashwo
 (GB : sig
         include Builder.S

         (** Assign weights to edges. *)
         val weight : G.edge -> weight
       end) :
sig
  (** Raised if cycles remain and all the remaining vertexes are obligatory.
      The argument gives the list of remaining vertexes. *)
  exception Stuck of GB.G.vertex list

  (** Return a minimal set of arcs whose removal or reversal would make the
      given graph acyclic.

      By minimal, we mean that each arc in the returned list must be removed
      or reversed, i.e., none are superfluous. Since a heuristic is used, the
      returned list may not be a minimum feedback arc set. Finding the {i
      minimum feedback arc set}, dually, the {i maximum acyclic subgraph} is
      NP-hard for general graphs. *)
  val feedback_arc_set : GB.G.t -> GB.G.edge list
end

