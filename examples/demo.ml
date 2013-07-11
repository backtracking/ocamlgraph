(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2007                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Rand

open Sys
open Printf
open Graph

let v_ = ref 30
let prob_ = ref 0.5
let seed_ = ref None

type algo = TransitiveClosure | Prim
let algo = ref None

let arg_spec =
  ["-v", Arg.Int (fun i -> v_ := i),
   " <int>  number of vertices";
   "--prob", Arg.Float (fun f -> prob_ := f),
   " <float>  probability to discrad an edge";
   "--seed", Arg.Int (fun n -> seed_ := Some n),
   " <int>  random seed";

   "--transitive-closure", Arg.Unit (fun () -> algo := Some TransitiveClosure),
   "  display new edges in blue";
   "--prim", Arg.Unit (fun () -> algo := Some Prim),
   "  Prim's algorithm"
  ]
let () = Arg.parse arg_spec (fun _ -> ()) "usage: color <options>"

let v = !v_
let prob = !prob_

let seed = match !seed_ with
  | None -> Random.self_init (); Random.int (1 lsl 29)
  | Some s -> s
let () = Format.printf "seed = %d@." seed; Random.init seed

module G = struct

  module IntInt = struct
    type t = int * int
    let compare = Pervasives.compare
    let equal = (=)
    let hash = Hashtbl.hash
  end
  module Int = struct
    type t = int
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal = (=)
    let default = 0
  end

  include Imperative.Digraph.ConcreteLabeled(IntInt)(Int)

end

(* a random graph with n vertices *)
module R = Rand.Planar.I(G)
let g = R.graph ~xrange:(20,780) ~yrange:(20,580)~prob v

module Draw = struct

  open Graphics
  let () = open_graph " 800x600"

  let vertex_radius = 5
  let round f = truncate (f +. 0.5)
  let pi = 4.0 *. atan 1.0

  let draw_arrow ?(color=black) ?(width=1) (xu,yu) (xv,yv) =
    set_color color;
    set_line_width width;
    let dx = float (xv - xu) in
    let dy = float (yv - yu) in
    let alpha = atan2 dy dx in
    let r = sqrt (dx *. dx +. dy *. dy) in
    let ra = float vertex_radius *. 1.5 in
    let d = float vertex_radius +. 3. in
    let xs, ys = float xu +. d *. dx /. r, float yu +. d *. dy /. r in
    let xd, yd = float xv -. d *. dx /. r, float yv -. d *. dy /. r in
    let coords theta =
      round (xd +. ra *. cos (pi +. alpha +. theta)),
      round (yd +. ra *. sin (pi +. alpha +. theta))
    in
    moveto (round xs) (round ys);
    lineto (round xd) (round yd);
    let x1,y1 = coords (pi /. 6.) in
    moveto (round xd) (round yd); lineto x1 y1;
    let x2,y2 = coords (-. pi /. 6.) in
    moveto (round xd) (round yd); lineto x2 y2

  let draw_edge ?color ?width v1 v2 =
    draw_arrow ?color ?width (G.V.label v1) (G.V.label v2)

  let draw_vertex ?(color=red) v =
    let (x,y) = G.V.label v in
    set_color color;
    draw_circle x y vertex_radius

  let color_vertex v color =
    let x,y = G.V.label v in
    set_color color;
    fill_circle x y vertex_radius

  let draw_graph g =
    clear_graph ();
    G.iter_vertex draw_vertex g;
    G.iter_edges draw_edge g

end

let () = Draw.draw_graph g
let () = ignore (Graphics.wait_next_event [Graphics.Key_pressed ])

let () = match !algo with
  | Some TransitiveClosure ->
      let module O = Oper.I(G) in
      let tg = O.transitive_closure g in
      G.iter_edges
	(fun v1 v2 ->
	  if not (G.mem_edge g v1 v2) then
	    Draw.draw_edge ~color:Graphics.blue v1 v2) tg
  | Some Prim ->
      assert false (* TODO *)
  | None -> ()

let () =
  ignore (Graphics.wait_next_event [Graphics.Key_pressed ]);
  Graphics.close_graph ()

(*
Local Variables:
compile-command: "make -C .. bin/demo.opt"
End:
*)
