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

(** View classes.

    Each optional function [cache_node], [cache_edge] and [cache_cluster] of
    this module may be used to indicate whether an element must be displayed
    immediatly (if the function returns [false]) or only cached and displayed
    latter (if the function returns [true]). By default, each function always
    returns [false]. It may be set for improving efficiency. *)

open DGraphModel
open DGraphViewItem
open GnoCanvas

(** Simple widget derived from the Gnome Canvas
    Supports zooming and scrolling. *)
class ['vertex, 'edge, 'cluster] view:
  ?cache_node:('vertex -> bool) ->
  ?cache_edge:('edge -> bool) ->
  ?cache_cluster:('cluster -> bool) ->
  GnomeCanvas.canvas Gtk.obj ->
  ('vertex, 'edge, 'cluster) DGraphModel.abstract_model ->
object
  inherit canvas
    
  (** Model from DGraphModel *)
  method model : ('vertex, 'edge, 'cluster) DGraphModel.abstract_model
    
  (** Getters *)
  method get_node : 'vertex -> 'vertex view_item
  method get_edge : 'edge -> 'edge view_item
  method get_cluster : 'cluster -> 'cluster view_item

  (** Iterators *)
  method iter_nodes:  ('vertex view_item -> unit) -> unit
  method iter_edges: ('vertex view_item -> 'vertex view_item -> unit) -> unit
  method iter_edges_e:  ('edge view_item -> unit) -> unit
  method iter_clusters: ('cluster view_item -> unit) -> unit

  method iter_succ: ('vertex view_item -> unit) -> 'vertex view_item -> unit
  method iter_pred: ('vertex view_item -> unit) -> 'vertex view_item -> unit
  method iter_succ_e: ('edge view_item -> unit) -> 'vertex view_item -> unit
  method iter_pred_e: ('edge view_item -> unit) -> 'vertex view_item -> unit

  (** Membership functions *)
  method mem_edge: 'vertex view_item -> 'vertex view_item -> bool
  method find_edge: 'vertex view_item -> 'vertex view_item -> 'edge view_item
  method src: 'edge view_item -> 'vertex view_item
  method dst: 'edge view_item -> 'vertex view_item

  method zoom_factor : float
  method zoom_to : float -> unit
  method zoom_in : unit -> unit
  method zoom_out : unit -> unit
  method adapt_zoom : unit -> unit

  method connect_highlighting_event: unit -> unit
  method private highlight: 'vertex view_item -> unit
  method private dehighlight: 'vertex view_item -> unit
end

val view:
  ?aa:bool (** Anti-aliasing *) ->
  ?cache_node:('vertex -> bool) ->
  ?cache_edge:('edge -> bool) ->
  ?cache_cluster:('cluster -> bool) ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) ->
  ?show:bool -> 
  ('vertex, 'edge, 'cluster) DGraphModel.abstract_model ->
  ('vertex, 'edge, 'cluster) view
  (** View as a Gnome Canvas. 
      Support zooming and scrolling. *)

(* Same widget augmented with highlighting, focus
    and the ability to drag the canvas (click'n hold)
*)
(* class ['vertex, 'edge, 'cluster] drag_view : *)
(*   GnomeCanvas.canvas Gtk.obj -> *)
(*   ('vertex, 'edge, 'cluster) DGraphModel.abstract_model -> *)
(*   ['vertex, 'edge, 'cluster] view *)

(* val drag_view : *)
(*   ?aa:bool -> (\** Anti aliasing *\) *)
(*   ('vertex, 'edge, 'cluster) DGraphModel.abstract_model -> *)
(*   ?border_width:int -> *)
(*   ?width:int -> *)
(*   ?height:int -> *)
(*   ?packing:(GObj.widget -> unit) -> *)
(*   ?show:bool -> unit *)
(*   -> ('vertex, 'edge, 'cluster) highlight_focus_view *)
