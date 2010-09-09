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
(*  Author:                                                               *)
(*    - Benoit Bataille  (benoit.bataille@gmail.com)                      *)
(*                                                                        *)
(**************************************************************************)

open Graph
open Pango

let ($) f x = f x

let set_if_none field value =
  match field with
    |None -> Some value
    |Some a -> Some a

let get_some = function None -> assert false | Some a -> a

type cluster = string

(* CALCULATE POSITION - SHARED CODE *)

type 'a geometry_info = {
  dimensions : ('a, float*float) Hashtbl.t;
  position : ('a, float*float) Hashtbl.t;
  mutable x_offset : float;
  mutable y_offset : int;
}

let get_position v geometry_info =
  try Hashtbl.find geometry_info.position v
  with Not_found -> assert false;;

let get_dimensions v geometry_info =
 try Hashtbl.find geometry_info.dimensions v
  with Not_found -> assert false;;

let set_offset geometry_info =
  geometry_info.y_offset <- 150;
  geometry_info.x_offset <-
    Hashtbl.fold (fun _ (w,_) maxw -> max w maxw)
    geometry_info.dimensions 0.;;

(* Calculate node positions for a tree *)
let fill_tree_positions tree root iter_fun fold_fun table geometry_info =
  let vertex_x_space = 10. in

  let stack = Stack.create () in
  let fill_stack tree root =
    let stack_queue = Queue.create () in
    let rec flush_queue queue =
      if not(Queue.is_empty queue) then begin
	let (elem,depth) = Queue.take queue in
	iter_fun (function v -> Queue.add (v,depth+1) queue) tree elem;
	Stack.push (elem,depth) stack;
	flush_queue queue
      end
    in
    Queue.add (root,0) stack_queue;
    flush_queue stack_queue;
  in fill_stack tree root;

  let offset = ref geometry_info.x_offset in
  let max_depth = snd (Stack.top stack) in
  let rec flush_stack stack =
    if not (Stack.is_empty stack) then begin
      let (elem,depth) = Stack.pop stack in
      if depth = max_depth then begin
	Hashtbl.add table elem (!offset, depth);
	offset := !offset +. geometry_info.x_offset +. vertex_x_space;
      end else begin
	let (sum,cpt) =
	  fold_fun (fun v (sum,cpt) ->
	    let (x,_) =
	      try Hashtbl.find table v with Not_found -> assert false
	    in
	    (sum +. x, cpt +. 1.))
	    tree
	    elem
	    (0.,0.)
	in
	Hashtbl.add table elem (sum /. cpt, depth)
      end;
      flush_stack stack
    end
  in
  flush_stack stack;;

(* Bind two tree position tables together *)
let bind_tree_tables forward_table backward_table root geometry_info =
  let adjx, adjy =
    try Hashtbl.find forward_table root
    with Not_found -> assert false
  in
  Hashtbl.iter
    (fun key (abs, ord) ->
      Hashtbl.add geometry_info.position key
	(abs-.adjx, float_of_int ((ord-adjy)*(-geometry_info.y_offset))))
    forward_table;
  let adjx, adjy =
    try Hashtbl.find backward_table root
    with Not_found -> assert false
  in
  Hashtbl.iter
    (fun key (abs,ord) ->
      Hashtbl.add geometry_info.position key
	(abs-.adjx, float_of_int ((ord-adjy)*geometry_info.y_offset)))
    backward_table;
  Hashtbl.remove geometry_info.position root;;

(* DRAW OPERATIONS *)

(* Convert an int in hexadecimal representing a color in rgb format to a
string prefixed by # *)
let string_color i = "#" ^ Printf.sprintf "%06X" i;;

(* Give an array of positions to draw a edge given positions and dimensions of
vertices *)
let edge_to_posarray src dst geometry_info =
  let (xsrc,ysrc) = get_position src geometry_info in
  let (_,hsrc) = get_dimensions src geometry_info in
  let (xdst,ydst) = get_position dst geometry_info in
  let (_,hdst) = get_dimensions dst geometry_info in
  let ystart = ysrc -. hsrc and yend = ydst +. hdst in
  let xdec = 0.4 *. (xdst -. xsrc) in
  let ydec = 0.4 *. (ydst -. ysrc) in
  [|(xsrc, ystart);(xsrc +. xdec, ystart +. ydec);
    (xdst -. xdec, yend -. ydec);(xdst, yend)|];;

(* Give an array to draw an arrow given start and end positions of the edge *)
let edge_to_arrow (x1,y1) (x2,y2) =
  let warrow = 4. in (*Half-width of the arrow *)
  let harrow = 10. in (* Height of the arrow *)
  let dx = x2 -. x1 and dy = y1 -. y2 in
  let d = sqrt (dx *. dx +. dy *. dy) in
  let xp1 = -. (harrow *. dx +. warrow *. dy) /. d +. x2 in
  let yp1 = (harrow *. dy -. warrow *. dx) /. d +. y2 in
  let xp2 = (warrow *. dy -. harrow *. dx) /. d +. x2 in
  let yp2 = (warrow *. dx +. harrow *. dy) /. d +. y2 in
  [
  XDotDraw.Filled_polygon [|(x2,y2);(xp1,yp1);(xp2,yp2)|]
  ];;

(* FROM GRAPH *)

module Make (Tree : Graphviz.GraphWithDotAttrs ) = struct

  (* PARSE VERTICES ATTRIBUTES *)

  type vattributes = {
    (* See graphviz.mli for the meaning of each options *)
    mutable color : int option;
    mutable fontcolor : int option;
    mutable fontname : string option;
    mutable fontsize : int option;
    mutable height : float option;
    mutable label : string option;
    mutable orientation : float option;
    mutable peripheries : int option;
    mutable regular : bool option;
    mutable shape : [ `Ellipse | `Box | `Circle | `Doublecircle | `Diamond
      | `Plaintext | `Record | `Polygon of int * float ] option;
    mutable style : [ `Filled | `Solid | `Dashed | `Dotted | `Bold
      | `Invis ] list;
    mutable width : float option;
    mutable fillcolor : int option
  }

  let rec attributes_list_to_vattributes vattrs l =
    match l with
      |[] -> ()
      |(`Color c) :: q ->
	vattrs.color <- set_if_none vattrs.color c;
	attributes_list_to_vattributes vattrs q
      |(`Fontcolor c) :: q ->
	vattrs.fontcolor <- set_if_none vattrs.fontcolor c;
	attributes_list_to_vattributes vattrs q
      |(`Fontname n) :: q ->
	vattrs.fontname <- set_if_none vattrs.fontname n;
	attributes_list_to_vattributes vattrs q
      |(`Fontsize s) :: q ->
	vattrs.fontsize <- set_if_none vattrs.fontsize s;
	attributes_list_to_vattributes vattrs q
      |(`Height h) :: q ->
	vattrs.height <- set_if_none vattrs.height h;
	attributes_list_to_vattributes vattrs q
      |(`Label label) :: q ->
	vattrs.label <- set_if_none vattrs.label label;
	attributes_list_to_vattributes vattrs q
      |(`Orientation o) :: q ->
	vattrs.orientation <- set_if_none vattrs.orientation o;
	attributes_list_to_vattributes vattrs q
      |(`Peripheries p) :: q ->
	vattrs.peripheries <- set_if_none vattrs.peripheries p;
	attributes_list_to_vattributes vattrs q
      |(`Regular r) :: q ->
	vattrs.regular <- set_if_none vattrs.regular r;
	attributes_list_to_vattributes vattrs q
      |(`Shape shape) :: q ->
	vattrs.shape <- set_if_none vattrs.shape shape;
	attributes_list_to_vattributes vattrs q
      |(`Style s) :: q ->
	vattrs.style <- s :: vattrs.style;
	attributes_list_to_vattributes vattrs q
      |(`Width w) :: q ->
	vattrs.width <- set_if_none vattrs.width w;
	attributes_list_to_vattributes vattrs q
      |(`Fillcolor c) :: q ->
	vattrs.fillcolor <- set_if_none vattrs.fillcolor c;
	attributes_list_to_vattributes vattrs q
      |_ :: q -> attributes_list_to_vattributes vattrs q

  let fill_vattributes tree vattributes =
    let vertex_to_vattrs v =
      let vattrs = {
	color = None;
	fontcolor = None;
	fontname = None;
	fontsize = None;
	height = None;
	label = None;
	orientation = None;
	peripheries = None;
	regular = None;
	shape = None;
	style = [];
	width = None;
	fillcolor = None
      } in
      let dgraph_layout_default =
	[ `Color 0xFFFFFF; `Fontcolor 0x000000; `Fontname "Sans";
	  `Fontsize 12; `Height 0.; `Label (Tree.vertex_name v);
	  `Orientation 0.; `Peripheries 1; `Regular false; `Shape `Ellipse;
	  `Width 0.; `Fillcolor 0xFFFFFF ]
      in
      attributes_list_to_vattributes vattrs
	((Tree.vertex_attributes v)@(Tree.default_vertex_attributes tree)
	@dgraph_layout_default);
      vattrs
    in Tree.iter_vertex
      (fun v -> Hashtbl.add vattributes v (vertex_to_vattrs v))
    tree;;

  (* PLACE VERTICES *)

  (* Calculate dimension of a string in pixel *)
  let calc_dimensions family ptsize ?(weight=`NORMAL) ?(style=`NORMAL)
  string_to_mesure context_obj =
    let width_margin = 20. and height_margin = 0. in
    let font_description = Pango.Font.from_string "" in
    Pango.Font.modify font_description
      ~family:family
      ~weight
      ~style
      ~size: (ptsize * Pango.scale)
    ();
    let context = GtkBase.Widget.create_pango_context context_obj in
    Pango.Context.set_font_description context font_description;
    let layout = Pango.Layout.create context in
    Pango.Layout.set_text layout string_to_mesure;
    let (width, height) = Pango.Layout.get_pixel_size layout in
    ((float_of_int width) +. width_margin,
    (float_of_int height) +. height_margin);;

    let fill_dimensions context tree vattributes geometry_info =
      let add_vertex_dimensions v =
	let vattrs =
	  try Hashtbl.find vattributes v
	  with Not_found -> assert false
	in
	let (minwidth,minheight) =
	  (get_some vattrs.width, get_some vattrs.height)
	in
	let (truewidth,trueheight) =
	  calc_dimensions (get_some vattrs.fontname) (get_some vattrs.fontsize)
	  (get_some vattrs.label) context
	in
	let width = max minwidth truewidth in
	let height = max minheight trueheight in
	Hashtbl.add geometry_info.dimensions v (width,height)
      in
      Tree.iter_vertex add_vertex_dimensions tree;;

  let fill_position tree root geometry_info =
    let forward_table = Hashtbl.create 100 in
    let backward_table = Hashtbl.create 100 in
    fill_tree_positions tree root Tree.iter_succ Tree.fold_succ forward_table
      geometry_info;
    fill_tree_positions tree root Tree.iter_pred Tree.fold_pred backward_table
      geometry_info;
    bind_tree_tables forward_table backward_table root geometry_info;;

  (* BUILD LAYOUT - ADD DRAW OPERATIONS *)

  let style_to_style_attr = function
    | `Filled -> XDotDraw.Filled
    | `Solid -> XDotDraw.Solid
    | `Dashed -> XDotDraw.Dashed
    | `Dotted -> XDotDraw.Dotted
    | `Bold -> XDotDraw.Bold
    | `Invis -> XDotDraw.Invisible;;

  (* FOR VERTEX *)

  let shape_to_operations v vattrs geometry_info shape =
    let (width,height) = (fun (a,b) -> (a /. 2., b))
      (get_dimensions v geometry_info)
    in
    let position = get_position v geometry_info in
    let filled = List.mem `Filled vattrs.style in
    match shape with
      | `Ellipse ->
	if filled then
	  [ XDotDraw.Filled_ellipse (position,width,height) ]
	else
	  [ XDotDraw.Unfilled_ellipse (position,width,height) ]
      | `Circle ->
	let diameter = max width height in
	if filled then
	  [XDotDraw.Filled_ellipse (position,diameter,diameter)]
	else
	  [XDotDraw.Unfilled_ellipse (position,diameter,diameter)]
      | `Doublecircle ->
	let diameter = max width height in
	let big_diameter = diameter +. 5. in
	(XDotDraw.Unfilled_ellipse (position,big_diameter,big_diameter)) ::
	[if filled then
	  (XDotDraw.Filled_ellipse (position,diameter,diameter))
	else
	  (XDotDraw.Unfilled_ellipse (position,diameter,diameter))]
      | `Box ->
	let (x,y) = position in
	let x1 = x -. width and x2 = x +. width in
	let y1 = y -. height and y2 = y +. height in
	let pos_array = [|(x1,y1);(x1,y2);(x2,y2);(x2,y1)|] in
	if filled then
	  [ XDotDraw.Filled_polygon pos_array ]
	else
	  [ XDotDraw.Unfilled_polygon pos_array ]
      | `Record ->
	let (x,y) = position in
	let x1 = x -. width and x2 = x +. width in
	let y1 = y -. height and y2 = y +. height in
	let pos_array = [|(x1,y1);(x1,y2);(x2,y2);(x2,y1)|] in
	if filled then
	  [ XDotDraw.Filled_polygon pos_array ]
	else
	  [ XDotDraw.Unfilled_polygon pos_array ]
      | `Diamond ->
	let (x,y) = position in
	let x1 = x -. width and x2 = x +. width in
	let y1 = y -. height and y2 = y +. height in
	let pos_array = [|(x,y1);(x1,y);(x,y2);(x2,y)|] in
	if filled then
	  [XDotDraw.Filled_polygon pos_array]
	else
	  [ XDotDraw.Unfilled_polygon pos_array ]
      |_ -> [ XDotDraw.Unfilled_ellipse ((0.,0.),0.,0.) ];;

  let vattrs_to_draw_operations v vattributes geometry_info =
    let vattrs =
      try Hashtbl.find vattributes v
      with Not_found -> assert false
    in
    let (width,height) = get_dimensions v geometry_info in
    let pos = get_position v geometry_info in
    (
      (* Vertex shape drawing *)
      XDotDraw.Pen_color (string_color (get_some vattrs.color)) ::
      XDotDraw.Style (List.map (style_to_style_attr) vattrs.style) ::
      (if List.mem `Filled vattrs.style then
	XDotDraw.Fill_color (string_color (get_some vattrs.fillcolor)) ::
	shape_to_operations v vattrs geometry_info (get_some vattrs.shape)
      else
	shape_to_operations v vattrs geometry_info (get_some vattrs.shape))
    ,
      (* Vertex label drawing *)
      XDotDraw.Pen_color
	(string_color (get_some vattrs.fontcolor)) ::
      XDotDraw.Font (float_of_int (get_some vattrs.fontsize),
	get_some vattrs.fontname) ::
      XDotDraw.Text (pos,XDotDraw.Center,width,get_some vattrs.label) ::
      []
    );;

  let vertex_to_node_layout v vattributes geometry_info =
    let (draw,ldraw) =
      vattrs_to_draw_operations v vattributes geometry_info
    in
    let (width,height) = get_dimensions v geometry_info in
    let (abs,ord) = get_position v geometry_info in
    {
      XDot.n_name = Tree.vertex_name v;
      XDot.n_pos = (abs,ord);
      XDot.n_bbox = ((abs,ord),(abs +. width, ord +. height));
      XDot.n_draw = draw;
      XDot.n_ldraw = ldraw
    };;

  (* FOR CLUSTER *)

  open Graphviz.DotAttributes

  let get_clusters tree =
    let clusters = Hashtbl.create 20 in
    Tree.iter_vertex
      (fun v -> let subgraph = Tree.get_subgraph v in
	match subgraph with
	| None -> ()
	| Some c -> Hashtbl.add clusters c v)
    tree;
    clusters;;

  let rec get_cluster_color = function
    |[] -> 0x000000
    |`Color c :: _ -> c
    |_ :: q -> get_cluster_color q;;

  let find_cluster_corners l geometry_info =
    let max_x_distance = 2. *. geometry_info.x_offset in
    let max_y_distance = 2. *. (float_of_int geometry_info.y_offset) in
    let rec find_corners l corners_array =
      let (minx,miny) = corners_array.(0) in
      let (maxx,maxy) = corners_array.(3) in
      match l with
	|[] -> corners_array
	|v :: tl ->
	  let (x,y) = get_position v geometry_info in
	  let (w,h) = get_dimensions v geometry_info in
	  let halfw = w /. 2. in
	  let x1 = x -. halfw and x2 = x +. halfw in
	  let y1 = y -. h and y2 = y +. h in
	  (* Should cluster be split in two *)
	  let x1_distance = minx -. x1 in
	  let x2_distance = x2 -. maxx in
	  let y1_distance = miny -. y1 in
	  let y2_distance = y2 -. maxy in
	  if x1_distance > max_x_distance || x2_distance > max_x_distance ||
	  y1_distance > max_y_distance || y2_distance > max_y_distance ||
	  ((x1_distance != 0. || x2_distance != 0.) &&
	  (y1_distance != 0. || y2_distance != 0.)) then
	    Array.append (find_corners tl corners_array) (find_corners tl
	    [|(x1,y1);(x1,y2);(x2,y2);(x2,y1)|])
	  else
	    let newminx = min x1 minx in
	    let newminy = min y1 miny in
	    let newmaxx = max x2 maxx in
	    let newmaxy = max y2 maxy in
	    find_corners tl [|(newminx,newminy);(newminx,newmaxy);
	      (newmaxx,newmaxy);(newmaxx,newminy)|]
    in
    match l with
      |[] -> [|(0.,0.);(0.,0.);(0.,0.);(0.,0.)|]
      |v :: q ->
	  let (x,y) = get_position v geometry_info in
	  let (w,h) = get_dimensions v geometry_info in
	  let halfw = w /. 2. in
	  let x1 = x -. halfw and x2 = x +. halfw in
	  let y1 = y -. h and y2 = y +. h in
	  find_corners q [|(x1,y1);(x1,y2);(x2,y2);(x2,y1)|];;

  let cluster_to_cluster_layout tree c clusters geometry_info =
    let border_padding = 10. in
    let vertices =
      try Hashtbl.find_all clusters c
      with Not_found -> assert false
    in
    let corners_array = find_cluster_corners vertices geometry_info in
    let add_padding corners_array =
      let (x1,y1) = corners_array.(0) in
      let (x2,y2) = corners_array.(3) in
      let x1_padded = x1 -. border_padding in
      let x2_padded = x2 +. border_padding in
      let y1_padded = y1 -. border_padding in
      let y2_padded = y2 +. border_padding in
      [|(x1_padded,y1_padded);(x1_padded,y2_padded);
      (x2_padded,y2_padded);(x2_padded,y1_padded)|]
    in
    let rec cut_corners_array corners_array =
      (* [JS 2010/09/09] does not work:
	 exponential time seems to be required! *)
      let length = Array.length corners_array in
      if length > 4 then
	XDotDraw.Unfilled_polygon (add_padding (Array.sub corners_array 0 4)) ::
	(cut_corners_array (Array.sub corners_array 4 (length-4)))
      else
	[ XDotDraw.Unfilled_polygon (add_padding corners_array) ]
    in
    let (x1,y1) = corners_array.(0) in
    let (x2,y2) = corners_array.(3) in
    {
      XDot.c_pos = ((x1 +. x2) /. 2., (y1 +. y2) /. 2.);
      XDot.c_bbox = ((x1,y1),(x2,y2));
      XDot.c_draw =
	XDotDraw.Pen_color
	  (string_color (get_cluster_color c.sg_attributes)) ::
	(*cut_corners_array corners_array*)[];
      XDot.c_ldraw = []
    };;

  let build_cluster_layouts tree geometry_info =
    let cluster_layouts = Hashtbl.create 30 in
    let clusters = get_clusters tree in
    let visited = ref [] in
    Hashtbl.iter (fun c _ -> if not (List.mem c !visited) then
      let c_layout = cluster_to_cluster_layout tree c clusters geometry_info in
      Hashtbl.add cluster_layouts c.sg_name c_layout)
    clusters;
    cluster_layouts;;

  (* FOR EDGE *)

  type eattributes = {
    (* See graphviz.mli for the meaning of each field *)
    mutable color : int option;
    mutable decorate : bool option;
    mutable dir : [ `Forward | `Back | `Both | `None ] option;
    mutable fontcolor : int option;
    mutable fontname : string option;
    mutable fontsize : int option;
    mutable label : string option;
    mutable labelfontcolor : int option;
    mutable labelfontname : string option;
    mutable labelfontsize : int option;
    mutable style : [ `Solid | `Dashed | `Dotted | `Bold | `Invis ] list
  }

  let rec attributes_list_to_eattributes eattrs = function
    |[] -> ()
    |(`Color c) :: q ->
      eattrs.color <- set_if_none eattrs.color c;
      attributes_list_to_eattributes eattrs q
    |(`Decorate d) :: q ->
      eattrs.decorate <- set_if_none eattrs.decorate d;
      attributes_list_to_eattributes eattrs q
    |(`Dir d) :: q ->
      eattrs.dir <- set_if_none eattrs.dir d;
      attributes_list_to_eattributes eattrs q
    |(`Fontcolor c) :: q ->
      eattrs.fontcolor <- set_if_none eattrs.fontcolor c;
      attributes_list_to_eattributes eattrs q
    |(`Fontname n) :: q ->
      eattrs.fontname <- set_if_none eattrs.fontname n;
      attributes_list_to_eattributes eattrs q
    |(`Fontsize s) :: q ->
      eattrs.fontsize <- set_if_none eattrs.fontsize s;
      attributes_list_to_eattributes eattrs q
    |(`Label l) :: q ->
      eattrs.label <- set_if_none eattrs.label l;
      attributes_list_to_eattributes eattrs q
    |(`Labelfontcolor c) :: q ->
      eattrs.fontcolor <- set_if_none eattrs.fontcolor c;
      attributes_list_to_eattributes eattrs q
    |(`Labelfontname n) :: q ->
      eattrs.labelfontname <- set_if_none eattrs.labelfontname n;
      attributes_list_to_eattributes eattrs q
    |(`Labelfontsize s) :: q ->
      eattrs.labelfontsize <- set_if_none eattrs.labelfontsize s;
      attributes_list_to_eattributes eattrs q
    |(`Style s) :: q ->
      eattrs.style <- s :: eattrs.style;
      attributes_list_to_eattributes eattrs q
    |_ :: q -> attributes_list_to_eattributes eattrs q;;

  let eattrs_to_operation tree e geometry_info =
    let eattrs = {
      color = None;
      decorate = None;
      dir = None;
      fontcolor = None;
      fontname = None;
      fontsize = None;
      label = None;
      labelfontcolor = None;
      labelfontname = None;
      labelfontsize = None;
      style = [] }
    in
    let dgraph_layout_default =
      [ `Color 0xFF0000; `Decorate false; `Dir `Forward; `Fontcolor 0x00000;
	`Fontname "Sans"; `Fontsize 12; `Label ""; `Labelfontcolor 0x000000;
	`Labelfontname "Sans"; `Labelfontsize 12; `Style `Solid ] in
    attributes_list_to_eattributes eattrs
      ((Tree.default_edge_attributes tree)@(Tree.edge_attributes e)@
      dgraph_layout_default);
    let posarray =
      edge_to_posarray (Tree.E.src e) (Tree.E.dst e) geometry_info
    in
    let xsrc,ysrc = posarray.(0) in
    let xend,yend = posarray.(3) in
    (
      (* Shapes and curves *)
      [ XDotDraw.Pen_color (string_color (get_some eattrs.color));
	XDotDraw.Fill_color (string_color (get_some eattrs.color));
	XDotDraw.Style (List.map (style_to_style_attr) eattrs.style);
	XDotDraw.Filled_bspline posarray ]
    ,
      (* Label drawing *)
      [ XDotDraw.Pen_color (string_color (get_some eattrs.fontcolor));
	XDotDraw.Fill_color (string_color (get_some eattrs.fontcolor));
	XDotDraw.Font (float_of_int (get_some eattrs.fontsize),
	(get_some eattrs.fontname));
	(let pos = ((xsrc +. xend) /. 2. +. 5., (ysrc +. yend) /. 2.) in
	XDotDraw.Text (pos,XDotDraw.Center,40.,get_some eattrs.label)) ]
    ,
      (* Head arrowhead drawing *)
      (if eattrs.dir = Some `None then
	[]
      else
	XDotDraw.Pen_color (string_color (get_some eattrs.color)) ::
	XDotDraw.Fill_color (string_color (get_some eattrs.color)) ::
	XDotDraw.Style (List.map (style_to_style_attr) eattrs.style) ::
	(edge_to_arrow posarray.(2) posarray.(3)))
    ,
      (* Tail arrowhead drawing *)
      []
    ,
      (* Head label drawing *)
      []
    ,
      (* Tail label drawing *)
      []
    );;

  let edge_to_edge_layout tree e geometry_info =
    let (draw,ldraw,hdraw,tdraw,hldraw,tldraw) =
      eattrs_to_operation tree e geometry_info in
    {
      XDot.e_draw = draw;
      XDot.e_ldraw = ldraw;
      XDot.e_hdraw = hdraw;
      XDot.e_tdraw = tdraw;
      XDot.e_hldraw = hldraw;
      XDot.e_tldraw = tldraw
    };;

  (* Graph *)
  let from_tree context tree root =
    let vattributes = Hashtbl.create 100 in
    fill_vattributes tree vattributes;
    let geometry_info = {
      dimensions = Hashtbl.create 100;
      position = Hashtbl.create 100;
      x_offset = 0.;
      y_offset = 0
    } in
    fill_dimensions context tree vattributes geometry_info;
    set_offset geometry_info;
    fill_position tree root geometry_info;

    let vertex_layouts = Hashtbl.create 100 in
    Tree.iter_vertex
      (fun v ->
	let n_layout = vertex_to_node_layout v vattributes geometry_info in
	Hashtbl.add vertex_layouts v n_layout)
    tree;

    let edge_layouts = Hashtbl.create 100 in
    Tree.iter_edges_e
      (fun e ->
	let e_layout = edge_to_edge_layout tree e geometry_info in
	Hashtbl.add edge_layouts e e_layout)
    tree;

    let cluster_layouts = Hashtbl.create 7
  (* [JS 2010/09/09] does not work *)
(*	build_cluster_layouts tree geometry_info*)
    in

    let (xroot,yroot) = get_position root geometry_info in

    {
      XDot.vertex_layouts = vertex_layouts;
      edge_layouts = edge_layouts;
      cluster_layouts = cluster_layouts;
      bbox = Hashtbl.fold
	  (fun _ (x,y) ((minx,maxy),(maxx,miny)) ->
	    ((min x minx, max y maxy),(max x maxx, min y miny)))
	geometry_info.position ((xroot,yroot),(xroot,yroot));
    };;

end

(* FROM MODEL *)
module type Tree = sig
  type t
  module V : sig
    type t
    type label
    val label : t -> label
  end
  module E : sig
    type t
    type label
    val src : t -> V.t
    val dst : t -> V.t
  end
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_edges_e : (E.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val iter_pred : (V.t -> unit) -> t -> V.t -> unit
  val fold_succ : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  val fold_pred : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
end

module MakeFromDotModel (Tree: Tree with
type V.label = DGraphModel.DotG.V.t and type E.label = unit) = struct

  (* POSITIONS *)
  let fill_dimensions model tree geometry_info =
    let corners pos_array = Array.fold_left
      (fun ((minx,maxy),(maxx,miny)) (x,y) ->
	((min minx x, max maxy y), (max maxx x,min miny y)))
      (pos_array.(0),pos_array.(0)) pos_array
    in
    let rec get_size operations =
      match operations with
	| [] -> (0.,0.)
	| XDotDraw.Unfilled_ellipse (_,w,h) :: _ -> (2. *. w, h)
	| XDotDraw.Filled_ellipse (_,w,h) :: _ -> (2. *. w, h)
	| XDotDraw.Unfilled_polygon pos_array :: _ ->
	    let ((minx,maxy),(maxx, miny)) = corners pos_array in
	    (maxx -. minx, maxy -. miny)
	| XDotDraw.Filled_polygon pos_array :: _ ->
	    let ((minx,maxy),(maxx, miny)) = corners pos_array in
	    (maxx -. minx, maxy -. miny)
	| _ :: tl -> get_size tl
    in
    Tree.iter_vertex (fun v ->
      let layout = model#get_vertex_layout (Tree.V.label v) in
      let (w,h) = get_size layout.XDot.n_draw in
      Hashtbl.add geometry_info.dimensions v (w,h)
    ) tree;;

  let fill_position tree root geometry_info =
    let forward_table = Hashtbl.create 100 in
    let backward_table = Hashtbl.create 100 in
    fill_tree_positions tree root Tree.iter_succ Tree.fold_succ forward_table
      geometry_info;
    fill_tree_positions tree root Tree.iter_pred Tree.fold_pred backward_table
      geometry_info;
    bind_tree_tables forward_table backward_table root geometry_info;;

  (* VERTICES *)
  let rec parse_n_draw_operations operations pos =
    match operations with
      | [] -> []
      | XDotDraw.Unfilled_ellipse (_, w, h) :: tl ->
	  XDotDraw.Unfilled_ellipse (pos, w, h) ::
	  (parse_n_draw_operations tl pos)
      | XDotDraw.Filled_ellipse (_, w, h) :: tl ->
	  XDotDraw.Filled_ellipse (pos, w, h) ::
	  (parse_n_draw_operations tl pos)
      | XDotDraw.Filled_polygon pts :: tl ->
	  let length = float_of_int (Array.length pts) in
	  let (oldabssum,oldordsum) =
	    Array.fold_left (fun (xsum,ysum) (x,y) -> (xsum+.x,ysum+.y))
	    (0.,0.) pts
	  in
	  let (oldabs,oldord) = (oldabssum /. length, oldordsum /. length) in
	  let (abs,ord) = pos in
	  XDotDraw.Filled_polygon
	  (Array.map (fun (x,y) -> (x-.oldabs+.abs,y-.oldord+.ord)) pts)
	  :: (parse_n_draw_operations tl pos)
      | XDotDraw.Unfilled_polygon pts :: tl ->
	  let length = float_of_int (Array.length pts) in
	  let (oldabssum,oldordsum) =
	    Array.fold_left (fun (xsum,ysum) (x,y) -> (xsum+.x,ysum+.y))
	    (0.,0.) pts
	  in
	  let (oldabs,oldord) = (oldabssum /. length, oldordsum /. length) in
	  let (abs,ord) = pos in
	  XDotDraw.Unfilled_polygon
	  (Array.map (fun (x,y) -> (x-.oldabs+.abs,y-.oldord+.ord)) pts)
	  :: (parse_n_draw_operations tl pos)
      | op :: tl -> op :: (parse_n_draw_operations tl pos);;

  let rec parse_n_ldraw_operations operations pos =
    match operations with
      | [] -> []
      | XDotDraw.Text (_, align, w, s) :: tl ->
	  XDotDraw.Text (pos, align, w, s) :: (parse_n_ldraw_operations tl pos)
      | op :: tl -> op :: (parse_n_ldraw_operations tl pos);;

  let parse_vertex_layout tree v layout geometry_info =
    let (width,height) = get_dimensions v geometry_info in
    let (abs,ord) = get_position v geometry_info in
    {
      XDot.n_name = layout.XDot.n_name;
      n_pos = (abs,ord);
      n_bbox = ((abs,ord),(abs +. width, ord +. height));
      n_draw =
	parse_n_draw_operations layout.XDot.n_draw (abs,ord);
      n_ldraw = parse_n_ldraw_operations layout.XDot.n_ldraw (abs,ord)
    };;

  (* EDGES *)
  let rec parse_e_draw_operations operations src dst geometry_info =
    match operations with
      | [] -> []
      | XDotDraw.Bspline _ :: tl ->
	  let pos_array = edge_to_posarray src dst geometry_info in
	  XDotDraw.Bspline pos_array ::
	  (edge_to_arrow pos_array.(2) pos_array.(3)) @
	  (parse_e_draw_operations tl src dst geometry_info)
      | XDotDraw.Filled_bspline _ :: tl ->
	  let pos_array = edge_to_posarray src dst geometry_info in
	  XDotDraw.Filled_bspline pos_array ::
	  (edge_to_arrow pos_array.(2) pos_array.(3)) @
	  (parse_e_draw_operations tl src dst geometry_info)
      | XDotDraw.Pen_color c :: tl ->
	  XDotDraw.Pen_color c :: XDotDraw.Fill_color c ::
	  (parse_e_draw_operations tl src dst geometry_info)
      | op :: tl -> op :: (parse_e_draw_operations tl src dst geometry_info);;

  let rec parse_e_ldraw_operations operations src dst geometry_info =
    match operations with
      | [] -> []
      | XDotDraw.Text (_, align, w, s) :: tl ->
	  let (xsrc,ysrc) = get_position src geometry_info in
	  let (xdst,ydst) = get_position dst geometry_info in
	  let pos = ((xsrc +. xdst) /. 2., (ysrc +. ydst) /. 2.) in
	  XDotDraw.Text (pos, align, w, s) ::
	  (parse_e_ldraw_operations tl src dst geometry_info)
      | op :: tl -> op :: (parse_e_ldraw_operations tl src dst geometry_info);;

  let parse_edge_layout tree e layout geometry_info =
    let src = Tree.E.src e and dst = Tree.E.dst e in
    {
      XDot.e_draw =
	parse_e_draw_operations layout.XDot.e_draw src dst geometry_info;
      e_ldraw =
	parse_e_ldraw_operations layout.XDot.e_ldraw src dst geometry_info;
      e_hdraw = [];
      e_tdraw = [];
      e_hldraw = [];
      e_tldraw = []
    };;

  (* CLUSTERS *)
  let parse_cluster_layout tree c global_layout geometry_info =
    ();;

  let from_model tree root model =

    let geometry_info = {
      dimensions = Hashtbl.create 100;
      position = Hashtbl.create 100;
      x_offset = 0.;
      y_offset = 0
    } in
    fill_dimensions model tree geometry_info;
    set_offset geometry_info;
    fill_position tree root geometry_info;

    let vertex_layouts = Hashtbl.create 100 in
    Tree.iter_vertex
      (fun v ->
	let old_layout = model#get_vertex_layout (Tree.V.label v) in
	let v_layout = parse_vertex_layout tree v old_layout geometry_info in
	Hashtbl.add vertex_layouts v v_layout)
    tree;

    let edge_layouts = Hashtbl.create 100 in
    Tree.iter_edges_e
      (fun e ->
	let src = Tree.V.label (Tree.E.src e) in
	let dst = Tree.V.label (Tree.E.dst e) in
	let old_layout =
	  try model#get_edge_layout (model#find_edge src dst)
	  with Not_found ->
	    {
	      XDot.e_draw = [];
	      e_ldraw = [];
	      e_hdraw = [];
	      e_tdraw = [];
	      e_hldraw = [];
	      e_tldraw = []
	    }
	in
	let e_layout = parse_edge_layout tree e old_layout geometry_info in
	Hashtbl.add edge_layouts e e_layout)
    tree;

    let cluster_layouts = Hashtbl.create 100 in

    let (xroot,yroot) = get_position root geometry_info in

    {
      XDot.vertex_layouts = vertex_layouts;
      XDot.edge_layouts = edge_layouts;
      XDot.cluster_layouts = cluster_layouts;
      bbox = Hashtbl.fold
	  (fun _ (x,y) ((minx,maxy),(maxx,miny)) ->
	    ((min x minx, max y maxy),(max x maxx, min y miny)))
	geometry_info.position ((xroot,yroot),(xroot,yroot));
    };;

end
