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

(* Copyright (c) 2012 Technische Universitaet Muenchen
 * Markus W. Weissmann <markus.weissmann@in.tum.de>
 * All rights reserved. *)

(** Edge contraction for directed, edge-labeled graphs *)

(* This algorithm should be extensible to undirected, unlabeled graphs! *)

(** Minimal graph signature for edge contraction algorithm *)
module type G = sig
  type t
  module V : Sig.COMPARABLE
  type vertex = V.t
  module E : Sig.EDGE with type vertex = vertex
  type edge = E.t

  val empty : t
  val add_edge_e : t -> edge -> t
  val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make
    (G : G) :
sig
  module S : Set.S with type elt = G.vertex
  module M : Map.S with type key = G.vertex

  val contract : (G.E.t -> bool) -> G.t -> G.t
  (** [contract p g] will perform edge contraction on the graph [g].
      The edges for which the property [p] holds/is true will get contracted:
      The resulting graph will not have these edges; the start- and end-node
      of these edges will get united. The result graph does not include nodes
      with no incoming or outgoing edges. *)

  (** As for {!contract} but additionally returns a mapping that associates
      each node in the original graph to the set of nodes with which it is
      contracted in the result graph. The minimum element of each such set
      is used as the representative of the set in the result graph. Nodes
      with no incoming or outgoing edges are present in the mapping even if
      they are omitted from the result graph. *)
  val contract' : (G.E.t -> bool) -> G.t -> G.t * S.t M.t
end

