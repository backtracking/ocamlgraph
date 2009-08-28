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

(** Abstract graph model *)

(** Simple layout types *)

(** 2D coordinates *)
type pos = float * float      

(** upper-left and bottom-right corners *)
type bounding_box = pos * pos (* bounding box   *)

(**
   Layout informations are parsed from xdot files
   (dot files with graphviz layout).

   Each node or edge layout thus contains several lists of
   drawing operations.

   See http://www.graphviz.org/doc/info/output.html#d:xdot 
   to understand the details of the layout informations.

*)

(** Each node has at least a position and a bounding box. *)
type node_layout = {
  n_pos : pos;                        (** Center position *)
  n_bbox   : bounding_box;            (** Bounding box *)
  n_draw   : XDotDraw.operation list; (** Shape drawing *)
  n_ldraw  : XDotDraw.operation list; (** Label drawing *)
}


type cluster_layout = {
  c_pos : pos;                        
  c_bbox   : bounding_box;            
  c_draw   : XDotDraw.operation list;
  c_ldraw  : XDotDraw.operation list;
}

type edge_layout = {
  e_draw   : XDotDraw.operation list; (** Shapes and curves *)
  e_ldraw  : XDotDraw.operation list; (** Label drawing *)
  e_hdraw  : XDotDraw.operation list; (** Head arrowhead drawing *)
  e_tdraw  : XDotDraw.operation list; (** Tail arrowhead drawing *)
  e_hldraw : XDotDraw.operation list; (** Head label drawing *)
  e_tldraw : XDotDraw.operation list; (** Tail label drawing *)
}

(** Creates a node layout *)
val mk_node_layout :
  pos:pos ->
  bbox:bounding_box ->
  draw:XDotDraw.operation list ->
  ldraw:XDotDraw.operation list ->
  node_layout

(** Creates a cluster layout *)
val mk_cluster_layout :
  pos:pos ->
  bbox:bounding_box ->
  draw:XDotDraw.operation list ->
  ldraw:XDotDraw.operation list ->
  cluster_layout

(** Creates an edge layout *)
val mk_edge_layout :
  draw:XDotDraw.operation list ->
  ldraw:XDotDraw.operation list ->
  hdraw:XDotDraw.operation list ->
  tdraw:XDotDraw.operation list ->
  hldraw:XDotDraw.operation list ->
  tldraw:XDotDraw.operation list ->
  edge_layout

(** Immutable graph model.
    Graph size, layout accessors, iterators and
    membership functions.
*)
class virtual ['vertex, 'edge, 'cluster] model :
  object
    method virtual bounding_box : bounding_box

    (** Layout accessors *)
    method virtual get_edge_layout : 'edge -> edge_layout
    method virtual get_vertex_layout : 'vertex -> node_layout
    method virtual get_cluster_layout : 'cluster -> cluster_layout

    (** Various iterators *)
    method virtual iter_edges : ('vertex -> 'vertex -> unit) -> unit
    method virtual iter_edges_e : ('edge -> unit) -> unit
    method virtual iter_pred : ('vertex -> unit) -> 'vertex -> unit
    method virtual iter_pred_e : ('edge -> unit) -> 'vertex -> unit
    method virtual iter_succ : ('vertex -> unit) -> 'vertex -> unit
    method virtual iter_succ_e : ('edge -> unit) -> 'vertex -> unit
    method virtual iter_vertex : ('vertex -> unit) -> unit
    method virtual iter_clusters : ('cluster -> unit) -> unit

    (** Membership functions *)
    method virtual find_edge : 'vertex -> 'vertex -> 'edge
    method virtual mem_edge : 'vertex -> 'vertex -> bool
    method virtual mem_edge_e : 'edge -> bool
    method virtual mem_vertex : 'vertex -> bool
  end
