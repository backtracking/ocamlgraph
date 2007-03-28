open Graph.Pack.Graph
open Outils_math
open Outils_tort
open Format

let _ = GMain.Main.init ()

let graph = parse_gml_file Sys.argv.(1)

exception Choose of V.t
 
let root = 
  try
    iter_vertex (fun v -> raise (Choose v)) graph;
    Format.eprintf "empty graph@."; exit 0
  with Choose v ->
    v


(* [step_from n] computes the best `distance' for solving the
   dictator's problem in the complex hyperbolic plane for [n]
   dictators.  In a half-plane, we have to use the distance
   given by [step_from (2*n)] or, better, the distance given
   by [step_from (2*max(3 n))]. *)
let step_from n =
  ath (tan (pi_over_4 -. pi/.float(2*n)))


(* [hspace_dist_sqr turtle] computes the square of the distance
   between the origin and the half-space in front of [turtle]. *)
let hspace_dist_sqr turtle =
  let (ax, ay) = turtle.pos
  and (dx, dy) = turtle.dir in
  if ax*.dx +. ay*.dy < 0.0 then 0.0 else
  begin
    let ux = dy and uy = -.dx in
    let alpha = ax*.ax +. ay*.ay
    and beta = 2.0*.(ax*.ux +. ay*.uy) in
    if beta = 0.0 then
      alpha
    else
      begin
	let gamma = (1.0 +. alpha)/.beta in
	let delta = gamma*.gamma -. 1.0 in
	let sol =
          if beta > 0.0
          then -.gamma +. sqrt(delta)
          else -.gamma -. sqrt(delta) in
	let (zx, zy) = translate (ax, ay) (ux*.sol, uy*.sol) in
	zx*.zx +. zy*.zy
      end
  end ;;

(*  A modifier pour GTK

  let draw_label v =
  draw_string (string_of_int (V.label v))
*)


let edge v w = mem_edge graph v w || mem_edge graph w v 

let make_subgraph l =
  let gl = create () in
  List.iter (fun v -> add_vertex gl v) l;
  List.iter 
    (fun v -> List.iter (fun w -> if edge v w then add_edge gl v w) l) 
   l; 
  (* TODO: efficacite *)
  gl

let order_children l =
  let gl = make_subgraph l in
  let scc = Components.scc_list gl in
  let order_component c =
    let gc = make_subgraph c in
    let v = match c with
      | v :: l ->
	  List.fold_left 
	    (fun m v -> if out_degree gc v < out_degree gc m then v else m)
	    v l
      | [] -> 
	  assert false
    in 
    let l = ref [] in
    Dfs.prefix_component (fun w -> l := w :: !l) gc v;
    !l
  in
  let scc = List.map order_component scc in
  List.flatten scc

let rlimit = 0.90 
let rlimit_sqr = rlimit *. rlimit

module Vset = Set.Make(V)
let vset_of_list = List.fold_left (fun s x -> Vset.add x s) Vset.empty


module H = Hashtbl.Make(V)

let pos = H.create 97

let rec draw_graph noeud tortue canvas =
  if hspace_dist_sqr tortue <= rlimit_sqr then
    begin
      H.add pos noeud (0,tortue);
      tmoveto_gtk tortue;
    (*  draw_label noeud; *)
      let l = succ graph noeud in 
      let l = List.filter (fun x -> not (H.mem pos x) ) l in
      let l = order_children l in
      let n = List.length l in
      if n > 0 then
	begin
	  let pas = step_from (max 3 n)
	  and angle = 2. *. pi /. (float n) in
	  let ll = draw_edges tortue pas angle canvas l  in
	  List.iter (fun (v,tv) -> H.add pos v (1,tv)) ll;      
	  List.iter 
	    (fun (w,tw) ->	   
	       let l = succ graph w in
     	       let l = List.filter (fun x -> not (H.mem pos x)) l in
	       let n = List.length l in
	       if n > 0 then
		 begin
		   let pas = step_from (max 3 n)
		   and angle =  pi /. (float n) in
		   let tw = turn_right tw ((pi -. angle) /. 2.) in
		   let l = draw_edges tw pas angle canvas l in
		   List.iter (fun (v,tv) -> H.add pos v (2,tv)) l
		 end) 
	    ll;
	  (* draw intern edges *)
	  H.iter 
	    (fun v (lv,tv) -> 
	       List.iter
		 (fun w ->
		    try
		      let lw,tw = H.find pos w in
		      if abs (lw - lv) <> 1 then begin tmoveto_gtk tv; tlineto_gtk tw "grey" canvas end
		    with Not_found ->
		      ()) 
		 (succ graph v))
	    pos
	
	end
    end
and  draw_edges t pas angle canvas= function
  | [] -> 
      []
  | v :: l -> 
      let tv = tdraw_edge_gtk t pas 10 "black" canvas in 
      if hspace_dist_sqr t <= rlimit_sqr
      then (  (*   draw_label v;*)
	     H.add pos v (1,t));

      let t = turn_left t angle in
      let list = (v,tv) :: draw_edges t pas angle canvas l  in
      draw_graph v tv canvas;
      list

let draw origine tortue canvas=
  H.clear pos;
  draw_graph  root tortue canvas

open Gobject.Data
let cols = new GTree.column_list
let name = cols#add string

let create_model () =
  let model = GTree.tree_store cols in
  iter_vertex
    (fun v -> 
      let row = model#append () in
      model#set ~row ~column:name (string_of_int (V.label v));
      iter_succ
	(fun w ->
          let row = model#append ~parent:row () in
          model#set ~row ~column:name (string_of_int (V.label w)))
	graph v)
    graph;
  model

let node_selection ~(model : GTree.tree_store) path =
  let row = model#get_iter path in
  let s = model#get ~row ~column:name in
  Format.eprintf "node_selection %s@." s

open GtkTree

let add_columns ~(view : GTree.view) ~model =
  let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
  let vc =
    GTree.view_column ~title:"Nodes" ~renderer:(renderer, ["text", name]) ()
  in
  (*vc#add_attribute renderer "background-gdk" bg;*)
  ignore (view#append_column vc);
  vc#set_sizing `FIXED;
  vc#set_fixed_width 100;
  view#selection#connect#after#changed ~callback:
    begin fun () ->
      List.iter
        (fun p -> node_selection ~model p)
	view#selection#get_selected_rows;
    end


(* Ouverture fenetre GTK *)
let window = GWindow.window ~border_width: 10 ~title:"GraphEd" ~position: `CENTER () 
let _ = window#connect#destroy~callback:GMain.Main.quit 
let h_box = GPack.hbox ~homogeneous:false ~spacing:30  ~packing:window#add ()
let sw = GBin.scrolled_window ~shadow_type:`ETCHED_IN ~hpolicy:`NEVER
  ~vpolicy:`AUTOMATIC ~packing:h_box#add () 
let model = create_model ()
let treeview = GTree.view ~model ~packing:sw#add ()
let () = treeview#set_rules_hint true
let () = treeview#selection#set_mode `MULTIPLE
let _ = add_columns ~view:treeview ~model
(*let _ = treeview#misc#connect#realize ~callback:treeview#expand_all*)
 
(* la zone d'affichage du graph, le canvas *)
let canvas = 
  GnoCanvas.canvas ~aa:true ~width:(int_of_float w) ~height:(int_of_float h) ~packing:h_box#add () 
and tortue =
  let (x,y) = from_tortue !origine in
  moveto_gtk x y;
  make_turtle !origine 0.0 

let () = canvas#set_scroll_region 0. 0. w h 

let canvas_root = canvas#root 
  
(* l'affichage de la fenetre principale *)
let () = window#show ()

let _ = draw origine tortue canvas_root



let () = GMain.Main.main ()



