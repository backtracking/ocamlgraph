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

(** Reading XDot files *)

open Graph
open GnomeCanvas

exception ParseError of string

(* MISCELLANEOUS FUNCTIONS *)

let rec take n = function
  | [] -> []
  | l when n = 0 -> []
  | h::t -> h::(take (n-1) t)

let suffix s i = try String.sub s i ((String.length s)-i) 
                 with Invalid_argument("String.sub") -> ""

let rec group_tuples = function
  | [] -> []
  | [x] -> []
  | h1::h2::t -> (h1, h2) :: group_tuples t

(** Splits a string with a separator
   returns a list of strings *)
let split c s = 
  let rec split_from n = 
    try let p = String.index_from s n c 
        in (String.sub s n (p-n)) :: (split_from (p+1)) 
    with Not_found -> [ suffix s n ] 
  in if s="" then [] else split_from 0 ;;

(** Converts a coordinate from the dot file to a coordinate on
 the canvas *)
let conv_coord (x,y) =
  let pad = 4 in
  let dot_ppi = 72. in
  let dot_png_ppi = 96. in
  let factor = dot_png_ppi /. dot_ppi in
  float_of_int (x + pad) *. factor, -. float_of_int (y + pad) *. factor

let read_pos s =
  Scanf.sscanf s "%d,%d" (fun x y -> (x,y))

(** Converts a bounding box of center (x,y), width w and height h
    from a Dot file to a pair of corners (lower left and upper right)
    in the canvas coordinate system

    see http://www.graphviz.org/mywiki/FaqCoordTransformation
    to understand the [pad] and [factor] variables.

    @param pos position of the center of the node, in points.
    @param w width of the node, in inch.
    @param h height of the node, in inch.
*)
let bounding_box (x,y) w h = 
  let dot_ppi = 72. (* number of pixels per inch on a display device *) in
  let dot_png_ppi = 96. (* number of pixels per inch on a display device *) in
  try
    (* let w = float_of_int w *. 2 /. 72. in *)
    (* let h = float_of_int h *. 2 /. 72. in *)
    let pad = 4 in
    let x = float_of_int (x + pad) in
    let y = float_of_int (y + pad) in
    let dx = w in
    let dy = h in
    let x1 = x -. dx in
    let y1 = y -. dy in
    let x2 = x +. dx in
    let y2 = y +. dy in
    let factor = dot_png_ppi /. dot_ppi in
    let x1 = x1 *. factor in
    let y1 = -. y1 *. factor in
    let x2 = x2 *. factor in
    let y2 = -. y2 *. factor in
    ((x1,y1),(x2,y2))
  with e -> 
    let s = Printexc.to_string e in
    failwith (Format.sprintf "compute_coord failed : %s@." s)

(* READING VERTEX LAYOUTS *)

let read_node_label = function
  | Dot_ast.Ident s
  | Dot_ast.Number s
  | Dot_ast.String s
  | Dot_ast.Html s -> s

let read_points c s = 
  let s' = suffix s (String.index s c) in
  let tokens = List.filter (fun s -> s <> "") (List.tl (split ' ' s')) in
  try match tokens with
    | [] -> None
    | n::t ->
      let n = int_of_string n in
      let ints = List.map int_of_string (take (n*2) t) in
      let points = List.map conv_coord (group_tuples ints) in
      Some points
  with Failure "int_of_string" -> None

(** Finds the attributes [pos], [width] and [height] of a node
    in the attribute list *)
let read_common_layout mk_layout attr_list =
  (* Fold on the attributes *)
  (* shape, position, width, height, color, filled *)
  let fold ((p,w,h, draw,ldraw) as attrs) = function
    | (Dot_ast.Ident "pos"), Some (Dot_ast.String s) ->
        (Some s), w, h, draw,ldraw
    | (Dot_ast.Ident "width"), Some (Dot_ast.String s) ->
        p, (Some s), h, draw,ldraw
    | (Dot_ast.Ident "height"), Some (Dot_ast.String s) ->
        p, w, (Some s), draw,ldraw
    | (Dot_ast.Ident "_draw_"), Some (Dot_ast.String draw) ->
	p,w,h, XDotDraw.parse draw, ldraw
    | (Dot_ast.Ident "_ldraw_"), Some (Dot_ast.String ldraw) ->
	p,w,h, draw, XDotDraw.parse ldraw
    | _ -> attrs in
	
  let fold_attr acc attr_list = 
    List.fold_left fold acc attr_list in
  let attrs = List.fold_left fold_attr (None, None, None, [], [])
    attr_list in

  (* Check if we have position, width and height *)
  match attrs with
    | Some pos, Some w, Some h, draw,ldraw->
	let coord = bounding_box (read_pos pos)
	  (float_of_string w) (float_of_string h) in
	(* Return the node model *)
	let (x,y) = conv_coord (read_pos pos) in
	mk_layout ~pos:(x,y) ~bbox:coord ~draw ~ldraw
    | _,_,_, draw, ldraw ->
	let pos = (0.,0.) in
	let bbox = (0.,0.),(0.,0.) in
	mk_layout ~pos ~bbox ~draw ~ldraw

let read_node_layout = read_common_layout DGraphModel.mk_node_layout
let read_cluster_layout = read_common_layout DGraphModel.mk_cluster_layout

(* READING EDGE LAYOUTS *)

(** Reads the spline control points of a curve in an xdot file
    example : "c 5 -black B 4 65 296 65 288 65 279 65 270 "
 *)

(* The edge drawing operations are in the following attributes :
   _hdraw_	Head arrowhead
   _tdraw_	Tail arrowhead
   _hldraw_	Head label
   _tldraw_	Tail label
*)

(** Gets the layout of an edge out of the dot ast *)
let read_edge_layout attr_list =
  let draw   = ref [] in
  let ldraw  = ref [] in
  let hdraw  = ref [] in
  let tdraw  = ref [] in
  let hldraw = ref [] in
  let tldraw = ref [] in
  let fill_draw_ops = function
    | (Dot_ast.Ident "_draw_"),   Some (Dot_ast.String s) ->
	draw   := XDotDraw.parse s
    | (Dot_ast.Ident "_ldraw_"),  Some (Dot_ast.String s) ->
	ldraw  := XDotDraw.parse s
    | (Dot_ast.Ident "_hdraw_"),  Some (Dot_ast.String s) ->
	hdraw  := XDotDraw.parse s
    | (Dot_ast.Ident "_tdraw_"),  Some (Dot_ast.String s) ->
	tdraw  := XDotDraw.parse s
    | (Dot_ast.Ident "_hldraw_"), Some (Dot_ast.String s) ->
	hldraw := XDotDraw.parse s
    | (Dot_ast.Ident "_tldraw_"), Some (Dot_ast.String s) ->
	tldraw := XDotDraw.parse s	  
    | _ -> () in
  List.iter (List.iter fill_draw_ops) attr_list;
  let draw, ldraw = !draw, !ldraw in
  let hdraw, tdraw, hldraw, tldraw = !hdraw, !tdraw, !hldraw, !tldraw in
  DGraphModel.mk_edge_layout ~draw ~ldraw ~hdraw ~tdraw ~hldraw ~tldraw

(* Computes the bounding box *)
let read_bounding_box str =
  let x1,y1,x2,y2 =
    Scanf.sscanf str "%d,%d,%d,%d"
      (fun a b c d -> a,b,c,d) in

  (* Convert coordinates to the display coordinates *)
  let x1,y1 = conv_coord (x1,y1) in
  let x2,y2 = conv_coord (x2,y2) in
  ((x1,y1), (x2,y2))
