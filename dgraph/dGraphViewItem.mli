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

open GnoCanvas

(** Shape properties *)
type common_p =
    [ `FILL_COLOR of string
    | `FILL_STIPPLE of Gdk.bitmap
    | `OUTLINE_COLOR of string
    | `WIDTH_UNITS of float ]

(** Properties set when the item is highlighted *)
type highlight_conf = {
  hl_shp : common_p list;
  hl_txt : GnomeCanvas.text_p list;
}

class type shape = object
  method connect : GnoCanvas.item_signals
  (** Sets properties *)
  method set : common_p list -> unit
  (** Undoes the last call to "set" *)
  method undo : unit -> unit
end

(** Derived text class
   Uses a properties queue to undo changes
   (when dehighlighting for instance).
*)

class graph_text :
  GnomeCanvas.text Gtk.obj ->
  size_points:float ->
  props:GnomeCanvas.text_p list ->
object
  inherit text
  (** Undoes the last call to "set" *)
  method undo : unit -> unit
  (** Resizes the text *)
  method resize : float -> unit
  (** Initial size in points *)
  method init_size : float
end

(*type 'a t = 'a constraint 'a = < f:int; ..>;;*)

class type common_view = object
  inherit canvas
  method zoom_factor : float
end

(** ViewItem class
    Group of shapes and texts
*)
class view_item :
  view:common_view ->
  pos:float * float ->
  ops_list:XDotDraw.operation list list ->
  hl_conf:highlight_conf ->
object
  inherit group

  method shapes : shape list
  method texts : graph_text list

  method iter_shapes : (shape -> unit) -> unit
  method iter_texts : (graph_text -> unit) -> unit

  method highlight : unit -> unit
  method dehighlight : unit -> unit
  method select : unit -> unit
  method deselect : unit -> unit

  method center : unit -> unit
  method move_to : float -> float -> unit
end


class type ['vertex] view_node =
  object
    inherit view_item
    method vertex : 'vertex
  end

class type ['edge] view_edge =
  object
    inherit view_item
    method edge : 'edge
  end

class type ['cluster] view_cluster =
  object
    inherit view_item
    method cluster : 'cluster
  end

val view_node :
  ?hl_conf:highlight_conf ->
  view:common_view ->
  vertex:'vertex ->
  layout:DGraphModel.node_layout ->
  unit ->
  'vertex view_node

val view_edge :
  ?hl_conf:highlight_conf ->
  view:common_view ->
  edge:'edge ->
  layout:DGraphModel.edge_layout ->
  unit ->
  'edge view_edge

val view_cluster :
  view:common_view ->
  cluster:'cluster ->
  layout:DGraphModel.cluster_layout ->
  unit ->
  'cluster view_cluster
