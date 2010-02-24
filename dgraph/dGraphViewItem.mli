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

(* Shape *)

(** Shape properties *)
type shape_p = [ `FILL_COLOR of string
               | `OUTLINE_COLOR of string
	       | `WIDTH_UNITS of float
	       | `DASH of float * float array ]

class type textshape = object
  method highlight: unit -> unit
  method dehighlight: unit -> unit
  method hide: unit -> unit
  method show: unit -> unit
  method lower_to_bottom: unit -> unit
  method connect: 
    < event : callback:(GnoCanvas.item_event -> bool) -> GtkSignal.id;
    after : GnoCanvas.item_signals;
    destroy : callback:(unit -> unit) -> GtkSignal.id; >
end

class type shape = object
  inherit GnoCanvas.base_item
  inherit textshape
  val obj : GnomeCanvas.item Gtk.obj
  method set: shape_p list -> unit
end

(* Text *)

(** Derived text class. *)
class graph_text :
  GnomeCanvas.text Gtk.obj ->
  size_points:float ->
  props:GnomeCanvas.text_p list ->
object
  inherit text
  inherit textshape
  method init_size: float
    (** Initial size in points *)
  method resize: float -> unit
end

(* View items *)

class type common_view = object
  inherit canvas
  method zoom_factor : float
  method adapt_zoom : unit -> unit
end

(** ViewItem class.
    Group of shapes and texts *)
class ['a ] view_item :
  cache:bool ->
  view:common_view ->
  pos:float * float ->
  ops_list:XDotDraw.operation list list ->
  item:'a ->
object
  inherit group
  method item: 'a
  method zoom_text: float -> unit
  method highlight : unit -> unit
  method dehighlight : unit -> unit
  method show : unit -> unit
  method hide : unit -> unit
  method center : unit -> unit
  method connect_event: callback:(GnoCanvas.item_event -> bool) -> unit
  method compute: unit -> unit (** apply all cached operations *)
  method lower_to_bottom: unit -> unit
end

val view_node:
  cache:bool ->
  view:common_view ->
  vertex:'vertex ->
  layout:XDot.node_layout ->
  unit ->
  'vertex view_item

val view_edge:
  cache:bool ->
  view:common_view ->
  edge:'edge ->
  layout:XDot.edge_layout ->
  unit ->
  'edge view_item

val view_cluster:
  cache:bool ->
  view:common_view ->
  cluster:'cluster ->
  layout:XDot.cluster_layout ->
  unit ->
  'cluster view_item
