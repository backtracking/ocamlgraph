(**************************************************************************)
(*                                                                        *)
(*  This file is part of OcamlGraph.                                      *)
(*                                                                        *)
(*  Copyright (C) 2009                                                    *)
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
(*    - Jean-Denis Koeck (jdkoeck@gmail.com)                              *)
(*    - Julien Signoles  (Julien.Signoles@cea.fr)                         *)
(*                                                                        *)
(**************************************************************************)

(** Reads layout information from xdot ASTs *)

open Graph

exception ParseError of string

(** Converts a coordinate from the dot file to a coordinate on
 the canvas *)
val conv_coord : int * int -> float * float
val bounding_box : (int * int) -> float -> float -> DGraphModel.bounding_box
val read_bounding_box : string -> DGraphModel.bounding_box

val read_node_layout : Dot_ast.attr list -> DGraphModel.node_layout
val read_edge_layout : Dot_ast.attr list -> DGraphModel.edge_layout
val read_cluster_layout : Dot_ast.attr list -> DGraphModel.cluster_layout
