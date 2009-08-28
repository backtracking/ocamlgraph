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

open DGraphModel
open XDotDraw
open Printf

let ($) f x = f x

(* Derived text class
   Uses a properties queue to undo changes
   (when dehighlighting for instance).
*)
class graph_text txt_obj ~size_points ~props =
object(self)
  inherit GnoCanvas.text txt_obj as text

  (* Properties queue *)
  val mutable props_q = [props]

  method init_size : float = size_points
    
  (* Sets the properties, pushes them on the queue *)
  method set props =
    props_q <- props :: props_q;
    text#set props

  (* Restores the previous properties *)
  method undo () =
    match props_q with
      | [] -> () | [_] -> ()
      | _current::previous::rest ->
	  props_q <- previous::rest;
	  self#set previous

  (* When changing the text size, we avoid pushing it on the queue.
     Undoing resizements is useless. *)
  method resize new_size =
    if new_size > 0. then
      let props = [`SIZE_POINTS new_size] in
      match props_q with
	| [] -> text#set props
	| h::t ->
	    (* Merge the new size with the current properties *)
	    props_q <- (h@props) :: t;
	    text#set props
end

(* Constructor copied and adapted from gnoCanvas.ml *)
let graph_text ?x ?y ?text ?font ?anchor ~size_points ?(props=[]) p =
  let unoption_list ~rest l =
    List.fold_right (fun o acc -> match o with Some v -> v :: acc | None -> acc) l rest in
  let props = unoption_list ~rest:props
      [ ( match x with None -> None | Some v -> Some (`X v) ) ;
	( match y with None -> None | Some v -> Some (`Y v) ) ;
	( match text with None -> None | Some v -> Some (`TEXT v) ) ;
	( match font with None -> None | Some v -> Some (`FONT v) ) ;
	( Some (`SIZE_POINTS size_points) ) ;
	( match anchor with None -> None | Some v -> Some (`ANCHOR v) ) ; 
      ] in
  let i = GnomeCanvas.Item.new_item p#as_group GnomeCanvas.Types.text in
  let o = new graph_text i ~size_points ~props in
  if props <> [] then o#set props ;
  o

(** FROM DOT LAYOUT TO VIEW ITEMS *)

(* Common properties
   Used when initializing items
*)
let pen_color draw_st = `OUTLINE_COLOR draw_st.XDotDraw.pen_color
let fill_color draw_st = `FILL_COLOR draw_st.XDotDraw.fill_color
let fill_stipple () =
  `FILL_STIPPLE (Gdk.Bitmap.create_from_data ~width:1 ~height:1 "\000")

(* Flattens an array of pair of coordinates into an array of coordinates *)
let flatten_points pts =
  let pts' = Array.make (2 * Array.length pts) 0. in
  for i=0 to Array.length pts - 1 do
    let (x,y) = XDot.conv_coord pts.(i) in
    pts'.(2*i)   <- x;
    pts'.(2*i+1) <- y;
  done;
  pts'


(* SHAPE CONSTRUCTORS *)

(* Ellipse, polygon and bpath canvas items do not share the same type
   in lablgtk2
   They are kept in separate type constructors along with their
   initial properties.
*)
type shape_t =
  | SEllipse of GnoCanvas.ellipse
  | SPolygon of GnoCanvas.polygon
  | SBSpline of GnoCanvas.bpath

let to_p = function
  | `FILL_COLOR    _ as f -> f
  | `OUTLINE_COLOR _ as o -> o
  | `WIDTH_UNITS   _ as w -> w
  | `FILL_STIPPLE  _ as f -> f
  | _ -> invalid_arg "to_p"

(* Common properties (used by canvas items ellipse, polygon and bpath) *)
type common_p = [ `FILL_COLOR of string
                | `OUTLINE_COLOR of string
		| `WIDTH_UNITS of float
		| `FILL_STIPPLE of Gdk.bitmap ]

(* Converts a property list to a common property list *)
let conv_props = List.map to_p

(* Shape class (either a rect, an ellipse, a polygon or a path)
   Uses a properties queue to undo changes
*)
class shape shape props = object(self)

  method private set_props (props : common_p list) =
    match shape with
      | SPolygon p -> ignore $ p#set (conv_props props)
      | SEllipse e -> ignore $ e#set (conv_props props)
      | SBSpline b -> ignore $ b#set (conv_props props)

  (* Properties queue *)
  val mutable props_q = [props]

  (* Sets the properties, pushes them on the queue *)
  method set props =
    props_q <- props :: props_q;
    self#set_props props

  (* Restores the previous properties *)
  method undo () =
    match props_q with
      | [] -> () | [_] -> ()
      | _current::previous::rest ->
	  props_q <- previous::rest;
	  self#set_props previous

  method connect =
    match shape with
    | SPolygon p -> p#connect
    | SEllipse e -> e#connect
    | SBSpline b -> b#connect

end

let ellipse draw_st group pos w h =
  let((x1,y1), (x2,y2)) = XDot.bounding_box pos w h in
  let props = [pen_color draw_st;
	       fill_color draw_st;
	       ] in
  let ellip = GnoCanvas.ellipse group ~x1 ~y1 ~x2 ~y2 ~props in
  ellip#lower_to_bottom ();
  new shape (SEllipse ellip) (conv_props props)

let polygon draw_st group pts =
  let props = [pen_color draw_st;
	       fill_color draw_st;
	       ] in
  let points = flatten_points pts in
  let poly = GnoCanvas.polygon group ~points ~props in
  poly#lower_to_bottom ();
  new shape (SPolygon poly) (conv_props props)

let pathdef = function
  | pts when List.length pts mod 3 = 1 ->
      (* Starting point *)
      let pathdef = GnomeCanvas.PathDef.new_path () in
      let (x0,y0) = List.hd pts in
      GnomeCanvas.PathDef.moveto pathdef x0 y0;
      
      (* Rest of the spline *)
      let rec curveto = function
	| (x1,y1)::(x2,y2)::(x3,y3)::t ->
	    GnomeCanvas.PathDef.curveto pathdef x1 y1 x2 y2 x3 y3;
	    curveto t
	| _ -> () in
      curveto (List.tl pts);
      pathdef
  | _ -> failwith "Cannot build pathdef"

let bspline draw_st group pts =
  let path = pathdef (List.map XDot.conv_coord (Array.to_list pts)) in
  let base_props = [pen_color draw_st] in
  
  let fold_sty_attr props = function
    | Dashed -> (`DASH (10., [|10.|])) :: props
    | Dotted -> (`DASH (50., [|3.|])) :: props
    | _ -> props in
  let props = List.fold_left fold_sty_attr base_props draw_st.style in
      
  let bpath = GnoCanvas.bpath group ~bpath:path
    ~props in
  bpath#lower_to_bottom ();
  new shape (SBSpline bpath) (conv_props base_props)

let text draw_st group pos align label =
  let (x,y) = XDot.conv_coord pos in
  let size_points,font = draw_st.XDotDraw.font in
  (* let font = match XDotDraw.split '-' font' with *)
  (*   | font::_ -> String.lowercase font *)
  (*   | _ -> "" in *)
  let props = [`FILL_COLOR draw_st.XDotDraw.pen_color] in
  (* printf "Font: %s, size: %d\n" font size'; *)
  let text = graph_text group
    ~x ~y ~text:label ~props ~anchor:`SOUTH
    ~size_points:10. ~font (*~font ~size_points*) in
 (* text#lower_to_bottom (); *)
  text

(* HIGHLIGHT CONFIGURATION *)

type highlight_conf = {
  (* highlight *)
  hl_shp : common_p list;
  hl_txt : GnomeCanvas.text_p list;
}

let empty_hl_conf =
  { hl_shp = [];
    hl_txt = [] }

let view_node_hl_conf color =
  { hl_shp = [`OUTLINE_COLOR color];
    hl_txt = [`FILL_COLOR color; (*`SIZE_POINTS t#size_points*)];
  }

let view_edge_hl_conf color =
  { hl_shp = [`OUTLINE_COLOR color; `FILL_COLOR color];
    hl_txt = [`FILL_COLOR color];
  }

class type common_view = object
  inherit GnoCanvas.canvas
  method zoom_factor : float
end

(* ITEMS *)

(* DGraph item
   Can be either a node or an edge
   Contains shapes and texts.
   Can be : highlighted and/or selected, dragged and dropped

   ~pos : center of the container
   ~ops_list : list of list of operations
   ~hl_conf : highlight configuration (see in DGraphViewItem)
*)
class view_item ~(view : common_view) ~pos ~ops_list ~hl_conf =
  object(self)
    inherit GnoCanvas.group view#root#as_group

    val mutable selected = false
    val mutable highlighted = false
    val mutable texts = []
    val mutable shapes = []
    val mutable callbacks = []

    val drag = false

    method texts : graph_text list = texts
    method shapes : shape list = shapes

    method iter_texts f =  List.iter f texts
    method iter_shapes f = List.iter f shapes

    method highlight () =
      if not highlighted then begin
	List.iter (fun s -> s#set hl_conf.hl_shp) self#shapes;
	List.iter (fun t -> t#set hl_conf.hl_txt) self#texts;
	highlighted <- true
      end

    method dehighlight () =
      if highlighted then begin
	List.iter (fun s -> s#undo ()) self#shapes;
	List.iter (fun t -> t#undo ()) self#texts;
	highlighted <- false
      end

    method select () =
      (* Shapes *)
      let select_shape s = s#set [`WIDTH_UNITS 3.] in
      List.iter select_shape self#shapes;
      
      (* Texts *)
      let select_text t = ignore $ t#set [`WEIGHT 800] in
      List.iter select_text self#texts;
      selected <- true

    method deselect () =
      (* Deselect shapes and texts *)
      List.iter (fun s -> s#undo ()) self#shapes;
      List.iter (fun t -> t#undo ()) self#texts;
      selected <- false
	
    method center () =
      let (x,y) = pos in
      let w = view#hadjustment#page_size /. view#zoom_factor in
      let h = view#vadjustment#page_size /. view#zoom_factor in
      let sx = x -. (w /. 2.) in
      let sy = y -. (h /. 2.) in
      let sx, sy = view#w2c ~wx:sx ~wy:sy in
      ignore $ view#scroll_to ~x:sx ~y:sy

    method move_to x y = self#set [`X x; `Y y]

    (* val mutable dragging = false *)

    (* method drag_ev = function *)
    (*   | `BUTTON_PRESS _ -> *)
    (* 	  printf "Button press on item\n"; *)
    (* 	  dragging <- true; *)
    (* 	  true *)
    (*   | `MOTION_NOTIFY m -> *)
    (* 	  if dragging then begin *)
    (* 	    let x,y = GdkEvent.Motion.x m, GdkEvent.Motion.y m in *)
    (* 	    let x', y' = canvas#window_to_world ~winx:x ~winy:y in *)
    (* 	    let x'', y'' = canvas#w2c_d ~wx:x' ~wy:y' in *)
    (* 	    self#move_to x'' y'' *)
    (* 	  end; *)
    (* 	  true *)
    (*   | `BUTTON_RELEASE _ -> *)
    (* 	  printf "Button release on item\n"; *)
    (* 	  dragging <- false; *)
    (* 	  true *)
    (*   | _ -> false *)

    (* Reads a list of list of operations
       Updates the shapes and texts lists
    *)
    method private read_operations_list operations_list =
      let read_op draw_st = function
	  (* Create shapes *)
	| XDotDraw.Filled_ellipse (pos, w, h)
	| XDotDraw.Unfilled_ellipse (pos, w, h) -> 
	    shapes <- ellipse draw_st self pos w h :: shapes
	| XDotDraw.Filled_polygon pts | XDotDraw.Unfilled_polygon pts ->
	    shapes <- polygon draw_st self pts :: shapes
	| XDotDraw.Bspline pts | XDotDraw.Filled_bspline pts ->
	    shapes <- bspline draw_st self pts :: shapes
	| XDotDraw.Text (pos, align, _, label) ->
	    (*let new_text =
	      let draw_st' = XDotDraw.copy_draw_st draw_st in
	      (fun () ->
	      let t = text draw_st' view pos align label in
	      texts <- t :: texts;
	      false) in
 	      ignore $
	      Glib.Idle.add ~prio:(Glib.int_of_priority `LOW) new_text;*)
	    let t = text draw_st self pos align label in
	    texts <- t :: texts
	| _ -> () in
      List.iter (draw_with read_op) operations_list

    initializer
      self#read_operations_list ops_list
  end  


(* VIEW_NODE *)

(* Inherits view item class *)
class ['vertex] view_node ~(view:common_view) ~vertex ~layout ~hl_conf =
  let pos = layout.n_pos in
  let ops_list = [layout.n_ldraw; layout.n_draw] in
object(self)
  inherit view_item ~view ~pos ~ops_list ~hl_conf
  method vertex : 'vertex = vertex
end

let view_node ?(hl_conf = view_node_hl_conf "red")
              ~view ~vertex ~layout () =
  new view_node ~view ~vertex ~layout ~hl_conf

(* VIEW_EDGE *)

(* Inherits view item class *)
class ['edge] view_edge ~(view : common_view) ~edge ~layout ~hl_conf =
  let pos = 0.,0. in
  let ops_list =
    [ layout.e_draw; layout.e_ldraw;
      layout.e_tldraw; layout.e_hldraw;
      layout.e_tdraw; layout.e_hdraw    ] in
object(self)
  inherit view_item ~view ~pos ~ops_list ~hl_conf
  
  method edge : 'edge = edge
end

let view_edge ?(hl_conf = view_edge_hl_conf "red")
              ~view ~edge ~layout () =
  new view_edge ~view ~edge ~layout ~hl_conf

(* VIEW_CLUSTER *)
class ['cluster] view_cluster ~(view : common_view) ~cluster ~layout =
  let pos = layout.c_pos in
  let ops_list = [layout.c_ldraw; layout.c_draw] in
  let hl_conf = empty_hl_conf in
object(self)
  inherit view_item ~view ~pos ~ops_list ~hl_conf

  method cluster : 'cluster = cluster
end

let view_cluster ~view ~cluster ~layout () =
  new view_cluster ~view ~cluster ~layout
