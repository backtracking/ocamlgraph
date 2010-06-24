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

open DGraphViewItem

let ($) f x = f x

(* ************************************************************************* *)
(** View from a model *)
(* ************************************************************************* *)

(* Widget derived from Gnome Canvas.
   Supports zooming and scrolling *)
class ['v, 'e, 'c] view ?delay_node ?delay_edge ?delay_cluster
  obj (model : ('v, 'e, 'c) DGraphModel.abstract_model)
  =
  let delay f v = match f with None -> false | Some f -> f v in
  let (x1, y2), (x2, y1) = model#bounding_box in
object(self)

  inherit GnoCanvas.canvas obj

  method model = model

  (* Hash tables from the model to the view items*)
  val node_hash : ('v, 'v view_item) Hashtbl.t = Hashtbl.create 17
  val edge_hash : ('e, 'e view_item) Hashtbl.t = Hashtbl.create 17
  val cluster_hash : ('c, 'c view_item) Hashtbl.t = Hashtbl.create 17

  (* Canvas items creation *)

  method private add_vertex vertex =
    try
      let layout = model#get_vertex_layout vertex in
      let item =
	view_node
	  ~delay:(delay delay_node vertex)
	  ~view:(self :> common_view) ~vertex ~layout ()
      in
      Hashtbl.add node_hash vertex item
    with Not_found ->
      assert false

  method private add_edge edge =
    try
      let layout = model#get_edge_layout edge in
      let item =
	view_edge
	  ~delay:(delay delay_edge edge)
	  ~view:(self:>common_view) ~edge ~layout ()
      in
      Hashtbl.add edge_hash edge item
    with Not_found ->
      assert false

  method private add_cluster cluster =
    let layout = model#get_cluster_layout cluster in
    let item =
      view_cluster
	~delay:(delay delay_cluster cluster)
	~view:(self :> common_view) ~cluster ~layout ()
    in
    Hashtbl.add cluster_hash cluster item

  (* From model to view items *)

  method get_node n =
    try Hashtbl.find node_hash n with Not_found -> assert false

  method get_edge e =
    try Hashtbl.find edge_hash e with Not_found -> assert false

  method get_cluster c =
    try Hashtbl.find cluster_hash c with Not_found -> assert false

  (* Iterate on nodes and edges *)
  method iter_nodes f = Hashtbl.iter (fun _ v -> f v) node_hash
  method iter_edges_e f = Hashtbl.iter (fun _ e -> f e) edge_hash
  method iter_clusters f = Hashtbl.iter (fun _ c -> f c) cluster_hash

  method iter_edges f =
    model#iter_edges (fun v1 v2 -> f (self#get_node v1) (self#get_node v2))

  (* Iterate on successors of a node *)
  method iter_succ f (node: 'v view_item) =
    let f' v = f (self#get_node v) in
    model#iter_succ f' node#item

  (* Iterate on predecessors of a node *)
  method iter_pred f (node: 'v view_item) =
    let f' v = f (self#get_node v) in
    model#iter_pred f' node#item

  method iter_succ_e f (node: 'v view_item) =
    let f' e = f (self#get_edge e) in
    model#iter_succ_e f' node#item

  method iter_pred_e f (node: 'v view_item) =
    let f' e = f (self#get_edge e) in
    model#iter_pred_e f' node#item

  (* Membership functions *)

  method mem_edge (n1:'v view_item) (n2:'v view_item) =
    model#mem_edge n1#item n2#item

  method find_edge (n1:'v view_item) (n2:'v view_item) =
    self#get_edge (model#find_edge n1#item n2#item)

  method src (e: 'e view_item) = self#get_node (model#src e#item)
  method dst (e: 'e view_item) = self#get_node (model#dst e#item)

  (* Zoom factor *)
  val mutable zoom_f = 1.
  method zoom_factor = zoom_f

  val mutable zoom_padding = 0.1
  method set_zoom_padding n = zoom_padding <- n

  method private set_zoom_f x = if x > 1e-10 then zoom_f <- x

  (* Zooms the canvas according to the zoom factor *)
  method private zoom () =
    self#iter_clusters (fun c -> c#zoom_text zoom_f);
    self#iter_nodes (fun n -> n#zoom_text zoom_f);
    self#iter_edges_e (fun e -> e#zoom_text zoom_f);
    self#set_pixels_per_unit zoom_f

  (* Zoom to a particular factor *)
  method zoom_to x =
    self#set_zoom_f x;
    self#zoom ()

  method zoom_in () = self#zoom_to (zoom_f +. zoom_padding *. zoom_f)
  method zoom_out () = self#zoom_to (zoom_f -. zoom_padding *. zoom_f)

  method adapt_zoom () =
    let (x1',y1') = self#w2c ~wx:x1 ~wy:y1 in
    let (x2',y2') = self#w2c ~wx:x2 ~wy:y2 in
    let w = self#hadjustment#page_size in
    let h = self#vadjustment#page_size in
    let w_zoom = 0.99 *. w /. float (x2' - x1') in
    let h_zoom = 0.99 *. h /. float (y2' - y1') in
    self#zoom_to (min 1. (min w_zoom h_zoom));
    ignore $ self#scroll_to ~x:x1' ~y:y1';

  (* EVENTS *)

  (* Zoom with the keys *)
  method private zoom_keys_ev ev =
    match GdkEvent.Key.keyval ev with
    | k when k = GdkKeysyms._KP_Subtract -> self#zoom_out (); true
    | k when k = GdkKeysyms._KP_Add -> self#zoom_in (); true
    | _ -> false

  (* Zoom with the mouse *)
  method private zoom_mouse_ev ev =
    match GdkEvent.Scroll.direction ev with
    | `UP -> self#zoom_in (); true
    | `DOWN -> self#zoom_out (); true
    | _ -> false

  method highlight ?color node =
    let h e = e#highlight ?color () in
    h node;
    self#iter_succ_e h node;
    self#iter_pred_e h node

  method dehighlight node =
    let h e = e#dehighlight () in
    h node;
    self#iter_succ_e h node;
    self#iter_pred_e h node

  method connect_highlighting_event () =
    let connect node =
      let callback = function
	| `MOTION_NOTIFY _ -> self#highlight node; false
	| `LEAVE_NOTIFY _ -> self#dehighlight node; false
	| _ -> false
      in
      node#connect_event ~callback
    in
    self#iter_nodes connect

  initializer
    (* Create and add items from the model vertices, edges and clusters *)
    model#iter_clusters self#add_cluster;
    model#iter_vertex self#add_vertex;
    model#iter_edges_e self#add_edge;
    (* Set up scroll region *)
    ignore $ self#set_scroll_region ~x1 ~y1 ~x2 ~y2;
    let x1', y1' = self#w2c ~wx:x1 ~wy:y1 in
    ignore $ self#scroll_to ~x:x1' ~y:y1';
    (* Attach zoom events *)
    ignore $ self#event#connect#key_press self#zoom_keys_ev;
    ignore $ self#event#connect#scroll self#zoom_mouse_ev;

end

(* Constructor copied from gnoCanvas.ml *)
let view
    ?(aa=false) ?delay_node ?delay_edge ?delay_cluster
    ?border_width ?width ?height ?packing ?show
    model =
  GContainer.pack_container []
    ~create:(fun pl ->
	       let w =
		 if aa then GnomeCanvas.Canvas.new_canvas_aa ()
		 else GnomeCanvas.Canvas.new_canvas ()
	       in
	       Gobject.set_params w pl;
	       new view ?delay_node ?delay_edge ?delay_cluster w model)
    ?border_width ?width ?height ?packing ?show
    ()

(*
(* VIEW CLASS AUGMENTED WIDTH DRAGGING *)
(* Not really working, not exported *)
class ['v, 'e, 'c] drag_view obj model =
object(self)
  inherit ['v, 'e, 'c] highlight_focus_view obj model

  val mutable drag = false
  val mutable prev_pos = (0.,0.)

  (* EVENTS *)

  method private drag_start button =
    if not drag then begin
      drag <- true;
      let wx,wy = GdkEvent.Button.x button, GdkEvent.Button.y button in
      let x,y = self#w2c_d ~wx ~wy in
      prev_pos <- x, y
    end;
    false

  method private drag_end _button =
    if drag then begin
      drag <- false;
    end;
    false

  method private drag_move motion =
    let wx',wy' = GdkEvent.Motion.x motion, GdkEvent.Motion.y motion in
    let x',y' = self#w2c_d ~wx:wx' ~wy:wy' in
    if drag then begin
      let x,y = prev_pos in
      let dx, dy = (x'-.x) , (y'-.y)  in
      let offx, offy = self#hadjustment#value, self#vadjustment#value in
      let f = self#zoom_factor in
      let dx_scroll = dx /. f in
      let dy_scroll = dy /. f in
      self#hadjustment#set_value (offx -. dx_scroll);
      self#vadjustment#set_value (offy -. dy_scroll);
    end;
    prev_pos <- (x',y');
    false

  initializer
    (* Attach drag events *)
    ignore $ self#event#connect#button_press self#drag_start;
    ignore $ self#event#connect#button_release self#drag_end;
    ignore $ self#event#connect#motion_notify self#drag_move
end

let drag_view ?(aa=false) model =
  GContainer.pack_container [] ~create:(fun pl ->
    let w =
      if aa then GnomeCanvas.Canvas.new_canvas_aa ()
      else GnomeCanvas.Canvas.new_canvas () in
    Gobject.set_params w pl;
    new highlight_focus_view w model)
*)
