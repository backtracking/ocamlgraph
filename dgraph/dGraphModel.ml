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

(* Layout types *)
type pos = float * float      (* coordinates *)
type bounding_box = pos * pos (* bounding box   *)

type node_layout = {
  n_pos : pos;                        
  n_bbox   : bounding_box;            
  n_draw   : XDotDraw.operation list;
  n_ldraw  : XDotDraw.operation list;
}

type cluster_layout = {
  c_pos : pos;                        
  c_bbox   : bounding_box;            
  c_draw   : XDotDraw.operation list;
  c_ldraw  : XDotDraw.operation list;
}

type edge_layout = {
  e_draw   : XDotDraw.operation list;
  e_ldraw  : XDotDraw.operation list;
  e_hdraw  : XDotDraw.operation list;
  e_tdraw  : XDotDraw.operation list;
  e_hldraw : XDotDraw.operation list;
  e_tldraw : XDotDraw.operation list;
}

let mk_node_layout ~pos ~bbox ~draw ~ldraw =
  { n_pos   = pos;
    n_bbox  = bbox;
    n_draw  = draw;
    n_ldraw = ldraw }

let mk_cluster_layout ~pos ~bbox ~draw ~ldraw =
  { c_pos   = pos;
    c_bbox  = bbox;
    c_draw  = draw;
    c_ldraw = ldraw }

let mk_edge_layout ~draw ~ldraw ~hdraw ~tdraw ~hldraw ~tldraw =
  { e_draw   = draw;
    e_ldraw  = ldraw;
    e_hdraw  = hdraw;
    e_tdraw  = tdraw;
    e_hldraw = hldraw;
    e_tldraw = tldraw;
  }

(* This graph model is for now immutable, no adding or removing nodes. *)

class virtual ['vertex, 'edge, 'cluster] model = object(self)

  (* Graph interface *)
 
  (* Iterators *)
  method virtual iter_edges : ('vertex -> 'vertex -> unit) -> unit
  method virtual iter_edges_e : ('edge -> unit) -> unit
  method virtual iter_pred : ('vertex -> unit) -> 'vertex -> unit
  method virtual iter_pred_e : ('edge -> unit) -> 'vertex -> unit
  method virtual iter_succ : ('vertex -> unit) -> 'vertex -> unit
  method virtual iter_succ_e : ('edge -> unit) -> 'vertex -> unit
  method virtual iter_vertex : ('vertex -> unit) -> unit
  method virtual iter_clusters : ('cluster -> unit) -> unit

  (* Membership functions *)
  method virtual find_edge : 'vertex -> 'vertex -> 'edge
  method virtual mem_edge : 'vertex -> 'vertex -> bool
  method virtual mem_edge_e : 'edge -> bool
  method virtual mem_vertex : 'vertex -> bool

  (* method virtual remove_edge : 'vertex -> 'vertex -> unit *)
  (* method virtual remove_vertex : 'vertex -> unit *)

  (* Dot layout interface *)

  method virtual bounding_box : ((float * float) * (float * float))

  method virtual get_vertex_layout : 'vertex -> node_layout
  (*method virtual set_vertex_layout : 'vertex -> node_layout -> unit*)

  method virtual get_edge_layout : 'edge -> edge_layout
  (*method virtual set_edge_layout : 'edge -> edge_layout -> unit*)

  method virtual get_cluster_layout : 'cluster -> cluster_layout

  (* Dot creation *)
  (* Attrs format : [_=_,_=_,...] *)

  (* method default_vertex_attrs : string *)
  (* method get_vertex_attrs : 'vertex -> string *)

  (* method default_edge_attrs : string *)
  (* method get_vertex_attrs : 'edge -> string *)

  (* (\* We need labels to create and read dot and xdot files *\) *)
  (* method virtual label : 'vertex -> string *)
  (* method virtual from_label : string -> 'vertex *)

end
