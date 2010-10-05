(**************************************************************************)
(*                                                                        *)
(*  This file is part of OcamlGraph.                                      *)
(*                                                                        *)
(*  Copyright (C) 2009-2010                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1, with a linking exception.                    *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the file ../LICENSE for more details.                             *)
(*                                                                        *)
(*  Authors:                                                              *)
(*    - Julien Signoles  (Julien.Signoles@cea.fr)                         *)
(*    - Jean-Denis Koeck (jdkoeck@gmail.com)                              *)
(*    - Benoit Bataille  (benoit.bataille@gmail.com)                      *)
(*                                                                        *)
(**************************************************************************)

open Graph

module type G = sig
  type t
  module V : sig
    type t
    type label
    val label : t -> label
    val hash : t -> int
    val equal : t -> t -> bool
  end
  module E : sig
    type t
  end
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit 
  val iter_pred : (V.t -> unit) -> t -> V.t -> unit
  val find_edge : t -> V.t -> V.t -> E.t
end

module type EDGE = sig
  type t
  type label
  type vertex
  val create : vertex -> label -> vertex -> t
  val label : t -> label
  val src : t -> vertex
  val dst : t -> vertex
end

module type Tree = sig
  type t
  module V : sig
    type t
    type label
    val create : label -> t
    val label : t -> label
  end
  module E : EDGE with type vertex = V.t
  val create : ?size:int -> unit -> t
  val add_vertex : t -> V.t -> unit
  val add_edge_e : t -> E.t -> unit
end

module Manipulate (G : G) (Tree : Tree with type V.label = G.V.t 
and type E.label = unit) : sig

  type tree
  val make : G.t -> G.V.t -> int -> int -> tree
  val get_structure : tree -> Tree.t 
  val get_root : tree -> Tree.V.t 
  val get_tree_vertices : G.V.t -> tree -> Tree.V.t list
  val is_ghost_node : Tree.V.t -> tree -> bool
  val is_ghost_edge : Tree.E.t -> tree -> bool
  exception Ghost_node
  val get_graph_vertex : Tree.V.t -> tree -> G.V.t

end

module FromDotModel (Tree : Tree with type V.label = DGraphModel.DotG.V.t
and type E.label = unit) : sig

  type tree
  val make : 
    (DGraphModel.DotG.V.t, DGraphModel.DotG.E.t, string)
    DGraphModel.abstract_model -> DGraphModel.DotG.V.t -> int -> int -> tree
  val get_structure : tree -> Tree.t 
  val get_root : tree -> Tree.V.t 
  val get_tree_vertices : DGraphModel.DotG.V.t -> tree -> Tree.V.t list
  val is_ghost_node : Tree.V.t -> tree -> bool
  val is_ghost_edge : Tree.E.t -> tree -> bool
  exception Ghost_node
  val get_graph_vertex : Tree.V.t -> tree -> DGraphModel.DotG.V.t

end
