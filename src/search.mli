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

(** {2 Path Search Algorithms}

    This module implements several algorithms to find paths in graphs,
    given a source vertex and a set of target vertices (described by
    a Boolean function [success] below).

    Many one-player games can be conveniently seen as graphs, and
    these algorithms can then be used to solve them. There is no need
    to build the entire graph (though we can in some cases, provided
    it fits in memory), as the graph is implicitely described some
    adjacency function ([fold_succ_e] below). The graph may even be
    infinite in some cases.
*)

(** {2 Minimal graph signature}

    Everything apart from [success] is compatible with {!Sig.G}.
    This way, you can use graph structures from OCamlGraph as follows:
    {[
      module G = struct
        include Pack.Digraph (* or any other *)
        let success g v = ...
      end
    ]}
*)
module type G = sig
  type t
  module V : Sig.COMPARABLE
  type vertex = V.t
  module E : sig
    type t
    val src: t -> V.t
    val dst: t -> V.t
  end
  type edge = E.t
  val fold_succ_e: (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a

  val success: t -> vertex -> bool
end

(** Depth-First Search *)

module DFS(G: G) : sig

  val search: G.t -> G.vertex -> G.vertex * G.edge list
  (** [search g start] searches a solution from vertex [start].
      If a solution is found, it is returned as a final vertex [f]
      (for which [success] is true) and a path from [start] to [f].

      If no solution exists, exception [Not_found] is raised when all
      reachable vertices are visited. *)

end

(** Breadth-First Search

    A breadth-first search from the initial vertex guarantees a
    path of minimal length, if any.

    Caveat: Breadth-first search may require a lot of memory.
    If this is an issue, consider other implementations below,
    such as Iterative Deepening Search.
*)

module BFS(G: G) : sig

  val search: G.t -> G.vertex -> G.vertex * G.edge list
  (** [search g start] searches a solution from vertex [start].
      If a solution is found, it is returned as a final vertex [f]
      (for which [success] is true) and a path from [start] to [f].

      If no solution exists, exception [Not_found] is raised when all
      reachable vertices are visited. *)

end

(** Iterative Deepening Depth-First Search

    An alternative to breadth-first search is to perform depth-first
    searches with a maximal depth, that is increased until we find a
    solution. In graphs that are tress, this can be asymptotically as
    good as breadth-first search, but it uses much less memory.

    Caveat: It runs forever if there is no successful path and
    reachable cycles.
*)

module IDS(G: G) : sig

  val search: G.t -> G.vertex -> G.vertex * G.edge list
  (** [search g start] searches a solution from vertex [start].
      If a solution is found, it is returned as a final vertex [f]
      (for which [success] is true) and a path from [start] to [f].

      If no solution exists, exception [Not_found] is raised when all
      reachable vertices are visited.

      Note: This implementation is not tail recursive. It uses a stack
      space proportional to the length of the solution.  This is
      usually not an issue, as a solution whose length would exhaust
      the stack is likely to take too much time to be found.
*)

end

(** {2 Graphs with cost} *)

(** Dijkstra's algorithm

    This is distinct from {!Path.Dijkstra} in that we do not provide
    a target vertex, but a [success] function. *)

module Dijkstra(G: G)(C: Sig.WEIGHT with type edge = G.E.t): sig

  val search: G.t -> G.vertex -> G.vertex * G.edge list * C.t
  (** [search g start] searches a solution from vertex [start].
      If a solution is found, it is returned as a final vertex [f]
      (for which [success] is true), a path from [start] to [f], and
      the total cost of that path.

      If no solution exists, exception [Not_found] is raised when all
      reachable vertices are visited. *)

end

(** {2 Graphs with cost and heuristic} *)

(** A* algorithm *)

module Astar(G: G)(C: Sig.WEIGHT with type edge = G.E.t)
                  (H: sig val heuristic: G.V.t -> C.t end): sig

  val search: G.t -> G.vertex -> G.vertex * G.edge list * C.t

end

(** {2 Auxiliary functions to manipulate paths} *)

module Path(G: G) : sig

  (** A valid path is a list of edges where each destination vertex
      is the source vertex of the next edge in the list.

      Caveat: In the following, we do not check that edges belong to
      the graph. (And we could not.) *)

  val final: G.vertex -> G.edge list -> G.vertex
  (** [final start path] returns the final vertex of a [path]
      starting from vertex [start].
      Raises [Invalid_argument] if [path] is not a valid path from [start]. *)

  val valid: G.vertex -> G.edge list -> bool
  (** [check start path] returns [true] if and only if [path] is a
      valid path from [start].m *)

  val solution: G.t -> G.vertex -> G.edge list -> bool
  (** [check g start path] returns [true] if and only if [path] is a
      valid path from [start] ending on a success vertex. *)

end

