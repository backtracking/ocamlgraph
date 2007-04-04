open Format
open Graph.Pack.Graph
open Outils_tort
open PosGraph
open Outils_math


let _ = GMain.Main.init ()
 





let graph = parse_gml_file Sys.argv.(1)

exception Choose of V.t

type t = V.t
type label = V.t
let label x = x
let string_of_label x = string_of_int (V.label x)


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










open Gobject.Data
let cols = new GTree.column_list
let name = cols#add string
let vertex = cols#add caml

let create_model () =
  let model = GTree.tree_store cols in
  iter_vertex
    (fun v -> 
      let row = model#append () in
      model#set ~row ~column:name (string_of_int (V.label v));
      model#set ~row ~column:vertex v;
      iter_succ
	(fun w ->
          let row = model#append ~parent:row () in
          model#set ~row ~column:name (string_of_int (V.label w)))
	graph v)
    graph;
  model

open GtkTree



(* Ouverture fenetre GTK *)
let window = GWindow.window ~border_width: 10 ~title:"GraphEd" ~position: `CENTER () 
let h_box = GPack.hbox ~homogeneous:false ~spacing:30  ~packing:window#add ()
let sw = GBin.scrolled_window ~shadow_type:`ETCHED_IN ~hpolicy:`NEVER
  ~vpolicy:`AUTOMATIC ~packing:h_box#add () 
let canvas = GnoCanvas.canvas ~aa:true ~width:(truncate w) ~height:(truncate h) ~packing:h_box#add () 
let canvas_root = canvas#root 




let root = 
  try
    iter_vertex (fun v -> raise (Choose v)) graph;
    Format.eprintf "empty graph@."; exit 0
  with Choose v ->
    ref v



module Vset = Set.Make(V)
let vset_of_list = List.fold_left (fun s x -> Vset.add x s) Vset.empty


module H = Hashtbl.Make(V)

(* table donnant pour chaque noeud sa profondeur et sa tortue *)
let pos = H.create 97


let step = ref 0



let rec draw_graph depth noeud tortue canvas =
  if hspace_dist_sqr tortue <= rlimit_sqr then
    begin
      H.add pos noeud (depth,tortue);
      tmoveto_gtk tortue;
      (* draw label *)
      let ellipse = tdraw_string_gtk tortue (string_of_label noeud) canvas in
      let sigs = ellipse#parent#connect in
      let _ = sigs#event (drag_label ellipse) in
      
      let l = succ graph noeud in 
      let l = List.filter (fun x -> not (H.mem pos x) ) l in
      List.iter (fun w -> H.add pos w (depth+1, tortue)) l;
      let l = order_children l in
      let n = List.length l in
      if n > 0 then
	begin
	  let pas = step_from (max 3 n)
	  and angle = (if depth = 0 then 2. else 1.) *. pi /. (float_of_int n) in
	  let tortue = if depth = 0 then tortue else turn_right tortue ((pi -. angle) /. 2.) in
	  let ll = draw_edges (depth+1) tortue pas angle canvas l  in
	  ()
	end
    end
  else Format.eprintf"je devrai pas etre la"

and draw_edges depth t pas angle canvas= function
  | [] -> 
      []
  | v :: l -> 
      let tv = tdraw_edge_gtk t pas 10 "black" canvas in 
      (*if hspace_dist_sqr t <= rlimit_sqr then H.add pos v (depth,tv);*)
      let t = turn_left t angle in
      draw_graph depth v tv canvas;
      (v,tv) :: draw_edges depth t pas angle canvas l

and drag_label item ev =
  begin match ev with
    | `ENTER_NOTIFY _ ->
	  item#set [ `FILL_COLOR "steelblue" ]
    | `LEAVE_NOTIFY ev ->
	let state = GdkEvent.Crossing.state ev in
	  if not (Gdk.Convert.test_modifier `BUTTON1 state)
	  then item#set [ `FILL_COLOR "grey" ; ]
    | `BUTTON_RELEASE ev ->
	item#parent#ungrab (GdkEvent.Button.time ev)
    | `MOTION_NOTIFY ev ->
	incr step;
	let state = GdkEvent.Motion.state ev in
	if Gdk.Convert.test_modifier `BUTTON1 state && !step mod 1=0 then 
	  begin
	    let curs = Gdk.Cursor.create `FLEUR in
	    item#parent#grab [`POINTER_MOTION; `BUTTON_RELEASE] curs 
	      (GdkEvent.Button.time ev);
	    let mx = GdkEvent.Motion.x ev in
	    let my = GdkEvent.Motion.y ev in
	    moveto_gtk (truncate mx) (truncate my);
	    Format.eprintf "drag_label %f,%f @." mx my;
	    item#parent#move ~x: mx ~y: my;
	    item#parent#set  [`X mx; `Y my];
	    let tor =
	      let (x,y) = !point_courant in
	      let (x,y) = ((float_of_int x), (float_of_int y)) in
	      moveto_gtk (truncate x) (truncate y);
	      make_turtle (to_tortue(!point_courant)) 0.0;
	    in
	    let l =  canvas_root#get_items in
	    Format.eprintf "il y a %d elements dans le canvas @." (List.length l);
	    List.iter (fun v -> v#destroy())l;
	  
	    draw tor canvas_root;
	    
	    
	  end
    | _ ->
	()
  end;
  true

and draw tortue canvas =
  H.clear pos;
  draw_graph 0 !root tortue canvas;
  (* draw intern edges *)
  iter_edges
    (fun v w ->
       try
	 let lv,tv = H.find pos v in
	 let lw,tw = H.find pos w in
	 if abs (lw - lv) <> 1 then begin tmoveto_gtk tv; tlineto_gtk tw "grey" canvas end
       with Not_found ->
	 ()) 
    graph


let node_selection ~(model : GTree.tree_store) path =
  let row = model#get_iter path in
  let v = model#get ~row ~column: vertex in
  root := v;
  let tortue =
    let (x,y) = from_tortue !origine in
    moveto_gtk x y;
    make_turtle !origine 0.0;
  in
  let l =  canvas_root#get_items in
  Format.eprintf "il y a %d elements dans le canvas @." (List.length l);
  List.iter (fun v -> v#destroy())l;
  draw tortue canvas_root

    
let add_columns ~(view : GTree.view) ~model =
  let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
  let vc =
    GTree.view_column ~title:"Nodes" ~renderer:(renderer, ["text", name]) ()
  in
  ignore (view#append_column vc);
  vc#set_sizing `FIXED;
  vc#set_fixed_width 100;
  view#selection#connect#after#changed ~callback:
    begin fun () ->
      List.iter
        (fun p -> node_selection ~model p)
	view#selection#get_selected_rows;
    end
    


let _ = window#connect#destroy~callback:GMain.Main.quit 


let model = create_model ()
let treeview = GTree.view ~model ~packing:sw#add ()
let () = treeview#set_rules_hint true
let () = treeview#selection#set_mode `MULTIPLE
let _ = add_columns ~view:treeview ~model
(*let _ = treeview#misc#connect#realize ~callback:treeview#expand_all*)
 
(* la zone d'affichage du graph, le canvas *)
let tortue =
  let (x,y) = from_tortue !origine in
  moveto_gtk x y;
  make_turtle !origine 0.0 

let () = canvas#set_scroll_region 0. 0. w h 

  
(* l'affichage de la fenetre principale *)
let () = window#show ()

let _ = draw  tortue canvas_root



let () = GMain.Main.main ()



