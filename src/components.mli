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

(** Strongly connected components. *)

(** Minimal graph signature required by {!Make}.
    Sub-signature of {!Sig.G}. *)
module type G = sig
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
end

(** Functor providing functions to compute strongly connected components of a
    graph. *)
module Make (G: G) : sig

  val scc : G.t -> int * (G.V.t -> int)
  (** [scc g] computes the strongly connected components of [g].
      The result is a pair [(n,f)] where [n] is the number of
      components. Components are numbered from [0] to [n-1], and
      [f] is a function mapping each vertex to its component
      number. In particular, [f u = f v] if and only if [u] and
      [v] are in the same component. Another property of the
      numbering is that components are numbered in a topological
      order: if there is an arc from [u] to [v], then [f u >= f v]

      Not tail-recursive.
      Complexity: O(V+E)
      The function returned has complexity O(1) *)

  val scc_array : G.t -> G.V.t list array
  (** [scc_array g] computes the strongly connected components of [g].
      Components are stored in the resulting array, indexed with a
      numbering with the same properties as for [scc] above. *)

  val scc_list : G.t -> G.V.t list list
  (** [scc_list g] computes the strongly connected components of [g].
      The result is a partition of the set of the vertices of [g].
      The [n]-th components is [(scc_array g).(n-1)]. *)

end

(** Connectivity in strongly connected directed graphs *)

module Connectivity (GB: Builder.S) : sig
  module S : Set.S with type elt = GB.G.vertex

  val strong_articulation_points : GB.G.t -> GB.G.vertex list
  (** Computes the strong articulation points of the given
      strongly connected directed graph. The result is undefined if the
      input graph is not directed and strongly connected.

      A strong articulation point is a vertex that when removed from the
      original graph disconnects that graph into two or more components.

      The implementation involves constructing the mirror image of the
      graph; for bidirectional graphs prefer {!module:BiConnectivity}.

      Implements the algorithm from Italiano, Laura, and Santaroni,
      TCS 447 (2012), "Finding strong bridges and strong articulation points
      in linear time".
      Complexity: O(V + E) *)

  val sstrong_articulation_points : GB.G.t -> S.t
  (** As for [strong_articulation_points] but returns a set. *)
end

module BiConnectivity (G: Sig.G) : sig

  module S : Set.S with type elt = G.vertex

  val strong_articulation_points : G.t -> G.vertex list
  (** Computes the strong articulation points of the given
      strongly connected directed  graph. The result is undefined if the
      input graph is not directed and strongly connected.

      A strong articulation point is a vertex that when removed from the
      original graph disconnects that graph into two or more components.

      The implementation traverses the graph by iterating over predecessors;
      for unidirectional graphs prefer {!module:Connectivity}.

      Implements the algorithm from Italiano, Laura, and Santaroni,
      TCS 447 (2012), "Finding strong bridges and strong articulation points
      in linear time".
      Complexity: O(V + E) *)

  val sstrong_articulation_points : G.t -> S.t
  (** As for [strong_articulation_points] but returns a set. *)
end

(** Connected components (for undirected graphs).
    The implementation uses union-find. Time complexity is (quasi) O(V+E).
    Space complexity is O(V). *)

module type U = sig
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_edges : (V.t -> V.t -> unit) -> t -> unit
end

module Undirected(G: U) : sig
  val components: G.t -> int * (G.V.t -> int)
  val components_array: G.t -> G.V.t list array
  val components_list: G.t -> G.V.t list list
end
