(*************************************************************************)
(*                                                                       *)
(* DGraph: a library to interact with graphs in OCaml and lablgtk2       *)
(*                                                                       *)
(* Copyright (C) 2009 - Anne Pacalet                                     *)
(*                                                                       *)
(* This software is free software; you can redistribute it and/or        *)
(* modify it under the terms of the GNU Library General Public           *)
(* License version 2, with the special exception on linking              *)
(* described in file LICENSE.                                            *)
(*                                                                       *)
(* This software is distributed in the hope that it will be useful,      *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                       *)
(*************************************************************************)

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
