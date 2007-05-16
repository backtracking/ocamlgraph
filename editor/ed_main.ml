open Format
open Graph

open Ed_hyper
open Ed_graph
open Ed_display
open Ed_draw

let debug = ref false
let trace f x = 
  try f x with e -> eprintf "TRACE: %s@." (Printexc.to_string e); raise e

let _ = GMain.Main.init ()

(* Model for the treeview on the left *)

module Model = struct

  open Gobject.Data
  let cols = new GTree.column_list
  let name = cols#add string
  let vertex = cols#add caml
    
  let model = GTree.tree_store cols

  let rows = H.create 97

  let find_row v =
    try 
      H.find rows v
    with Not_found -> 
      Format.eprintf "anomaly: no model row for %s@." (string_of_label v);
      raise Not_found

  let add_vertex v =
    let row = model#append () in
    model#set ~row ~column:name (string_of_label v);
    model#set ~row ~column:vertex v;
    H.add rows v row;
    row

  let add_edge_1 row_v w =
    let row = model#append ~parent:row_v () in
    model#set ~row ~column:name (string_of_label w)

  let reset () =
    H.clear rows;
    model#clear ();
    G.iter_vertex
      (fun v -> 
	 let row = add_vertex v in
	 G.iter_succ (add_edge_1 row) !graph v)
      !graph

  let add_edge v w =
    let row_v = find_row v in
    add_edge_1 row_v w;
    if not G.is_directed then 
      let row_w = find_row w in
      add_edge_1 row_w v
      
end

let () = Model.reset ()

open GtkTree


(* Main GTK window *)
let window = 
  GWindow.window ~border_width: 10 ~title:"Editor" ~position: `CENTER () 

(* menu *)
let v_box = GPack.vbox ~homogeneous:false ~spacing:30  ~packing:window#add ()
let menu_bar = GMenu.menu_bar ~packing:v_box#pack () 

(* treeview on the left, canvas on the right *)
let h_box = GPack.hbox ~homogeneous:false ~spacing:30  ~packing:v_box#add ()
let sw = GBin.scrolled_window ~shadow_type:`ETCHED_IN ~hpolicy:`NEVER
  ~vpolicy:`AUTOMATIC ~packing:h_box#add () 
    
let canvas = 
  GnoCanvas.canvas ~aa:true ~width:(truncate w) ~height:(truncate h) 
    ~packing:h_box#add () 

let canvas_root = canvas#root 




let canvas_root =
  let circle_group = GnoCanvas.group ~x:300.0 ~y:300.0 canvas_root in
  circle_group#lower_to_bottom ();
  let w2 = 2. in
  let circle =
    GnoCanvas.ellipse  ~props:[ `X1 (-.w/.2. +.w2); `Y1 (-.h/.2. +.w2); 
				`X2  (w/.2. -.w2) ; `Y2 ( h/.2. -.w2) ;
 				`FILL_COLOR "white" ; `OUTLINE_COLOR "black" ; 
				`WIDTH_PIXELS (truncate w2) ] circle_group 
  in
  circle_group#lower_to_bottom ();
  circle#parent#show();
  let graph_root = GnoCanvas.group ~x:(-.300.0) ~y:(-.300.0) circle_group in
  graph_root#raise_to_top ();
  graph_root


(* selected node List *)
let vertex_selection = ref []
let is_selected x = 
  List.exists (fun (v,_) -> G.V.equal v x) !vertex_selection


(* current root used for drawing *)
let root = ref (choose_root ())


let load_graph f =
  Ed_graph.load_graph f;
  Model.reset ();
  root := choose_root ()



(* refresh rate *)
let refresh = ref 0
let do_refresh () =
  !refresh mod !refresh_rate = 0 



(* graph drawing *)
let draw tortue canvas =
 (* canvas#hide();*)
  Ed_draw.draw_graph !root tortue;
  Ed_display.draw_graph !root canvas;
  if do_refresh () then
    canvas_root#canvas#update_now ()
(*  canvas#show()*)




(* events *)

let node_selection ~(model : GTree.tree_store) path =
  let row = model#get_iter path in
  let v = model#get ~row ~column: Model.vertex in
  root := v;
  origine := start_point;
  let turtle = make_origine_turtle () in
  let l_item = canvas_root#get_items in
  Format.eprintf "il y a %d elements dans le canvas @." (List.length l_item);
  draw turtle canvas_root


let color_change_selection () =
  List.iter color_change_selected !vertex_selection


(* add an edge between n1 and n2 , add link in column and re-draw *)
let add_edge n1 n2 = 
  if not (edge n1 n2)
  then begin
    G.add_edge !graph n1 n2;
    Model.add_edge n1 n2;
    let tor = make_turtle !origine 0.0 in
    draw tor canvas_root
  end

let set_vertex_event_fun = ref (fun _ -> ())

(* add successor node to selected node *)
let add_successor node () =
  let window = GWindow.window ~title: "Choose label name" ~width: 300 ~height: 50 () in
  let vbox = GPack.vbox ~packing: window#add () in
  
  let entry = GEdit.entry ~max_length: 50 ~packing: vbox#add () in
  entry#set_text "Label";
  entry#select_region ~start:0 ~stop:entry#text_length;
  window#show ();
  let _ = entry#connect#activate 
    ~callback: (fun () ->
		  let text = entry#text in
		  window#destroy ();
		  (* new vertex *)
		  let vertex = G.V.create (make_node_info text)  in
		  G.add_vertex !graph  vertex ;
		  ignore (Model.add_vertex vertex);
		  Ed_display.add_node canvas_root vertex;
		  !set_vertex_event_fun vertex;
		  (* new edge *)
		  G.add_edge !graph node vertex;
		  Model.add_edge node vertex;
		  (* redraw *)
		  let tor = make_turtle !origine 0.0 in
		  draw tor canvas_root)
  in
  ()



let select_node node item=
  begin
    vertex_selection :=  (node, item)::!vertex_selection;
    color_change_selection ()
  end
  
let unselect_node_no_color node item =  
  vertex_selection := 
    List.filter (fun (v,_) -> not (G.V.equal v node)) !vertex_selection

let unselect_node node item =  
  color_change_selected (node,item);
  unselect_node_no_color node item;
  color_change_selection ();
  color_change_focused (node,item)


let unselect_all () =  
  List.iter (fun (node,item) -> 
	       unselect_node_no_color node item;
	       color_change_no_event (node,item)
	    )
    !vertex_selection;
  color_change_selection ()
    

let s_if_many = function
  | [] | [_] -> ""
  | _ -> "s"

let contextual_menu node ev =
  let loc_menu = GMenu.menu () in
  let factory = new GMenu.factory loc_menu in
  ignore (factory#add_item "  Add successor" ~callback: (add_successor node));
  begin match !vertex_selection with
    | [] -> ()
    | l ->
	ignore 
	  (factory#add_item ("  Add edge" ^ s_if_many l)
	      ~callback:(fun () -> 
		List.iter 
		  (fun (v,_) -> if not (G.V.equal v node) then add_edge v node)
		  l));
	
  end;
(***
  begin match !vertex_selection with
    | [] -> ()
    | (v,i)::lv -> 
	if not (G.V.equal v node)
	then begin
	  ignore (factory#add_item "  Add an edge" ~callback: (add_edge v node));	    
	end 
  end;
***)
  loc_menu#popup
    ~button:3
    ~time:(GdkEvent.Button.time ev)
    


(* event for each vertex of canvas *)
let vertex_event node item ev =
  begin match ev with
    | `ENTER_NOTIFY _ ->
	if  not (is_selected node)
	then begin
	  color_change_selection ();
	  color_change_focused (node,item)	
	end;

    | `LEAVE_NOTIFY ev ->
	if not (is_selected node)
	  && not (Gdk.Convert.test_modifier `BUTTON1 (GdkEvent.Crossing.state ev))
	then begin	
	  color_change_no_event (node,item);
	  color_change_selection ()
	end

    | `BUTTON_RELEASE ev ->
	item#parent#ungrab (GdkEvent.Button.time ev);

    | `MOTION_NOTIFY ev ->
	incr refresh;
	let state = GdkEvent.Motion.state ev in
	if Gdk.Convert.test_modifier `BUTTON1 state && do_refresh () then 
	  begin
	    let curs = Gdk.Cursor.create `FLEUR in
	    item#parent#grab [`POINTER_MOTION; `BUTTON_RELEASE] curs (GdkEvent.Button.time ev);
	    let turtle = motion_turtle item ev in
	    if hspace_dist_sqr turtle <= rlimit_sqr  then begin
	      draw turtle canvas_root;
	    end 
	  end

    | `BUTTON_PRESS ev ->
 	if (GdkEvent.Button.button ev) = 3
        then
	  begin
	    contextual_menu node ev
          end
	    
    | `TWO_BUTTON_PRESS ev->
      if (GdkEvent.Button.button ev) = 1
      then begin
	if not (is_selected node)
	then select_node node item
	else unselect_node node item
      end

    | `THREE_BUTTON_PRESS ev->
      if (GdkEvent.Button.button ev) = 1
      then begin
	unselect_all ();
	color_change_selected (node,item);
	color_change_focused (node,item)
      end

    | _ ->
	()
  end;
  true

let set_vertex_event v =
  let item,ell,_ = H.find nodes v in
  ignore (item#connect#event (vertex_event v ell))

let () = set_vertex_event_fun := set_vertex_event

let set_canvas_event () =
  (* vertex event *)
  G.iter_vertex set_vertex_event !graph


(* treeview *)
let add_columns ~(view : GTree.view) ~model =
  let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
  let vc =
    GTree.view_column ~title:"Nodes" ~renderer:(renderer, ["text", Model.name]) ()
  in
  ignore (view#append_column vc);
  vc#set_sizing `FIXED;
  vc#set_fixed_width 100;
(*  vc#set_resizable true;*)
vc#set_sizing `GROW_ONLY;
  view#selection#connect#after#changed ~callback:
    begin fun () ->
      List.iter
        (fun p -> node_selection ~model p)
	view#selection#get_selected_rows;
    end
    


let _ = window#connect#destroy~callback:GMain.Main.quit 


let treeview = GTree.view ~model:Model.model ~packing:sw#add ()
let () = treeview#set_rules_hint true
let () = treeview#selection#set_mode `MULTIPLE
let _ = add_columns ~view:treeview ~model:Model.model
(*let _ = treeview#misc#connect#realize ~callback:treeview#expand_all*)



(* reset *)

let reset_table_and_canvas () =
  let l =  canvas_root#get_items in
  List.iter (fun v -> trace v#destroy ()) l;
  H2.clear intern_edges;
  H2.clear successor_edges;
  reset_display canvas_root;
  origine := start_point;
  vertex_selection := []


(* menu *)
let create_menu label menubar =
  let item = GMenu.menu_item ~label ~packing:menubar#append () in
  GMenu.menu ~packing:item#set_submenu ()
    
let print msg () =
  print_endline msg;
  flush stdout


let default d = function
  | None -> d
  | Some v -> v
      
let all_files () =
  let f = GFile.filter ~name:"All" () in
  f#add_pattern "*" ;
  f
  
let graph_filter () = 
  GFile.filter 
    ~name:"Fichier de graphes" 
    ~patterns:[ "*.dot"; "*.gml" ] ()
  
let ask_for_file parent =
  let dialog = GWindow.file_chooser_dialog 
    ~action:`OPEN 
    ~title:"Ouvrir un fichier"
    ~parent () in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
  dialog#add_filter (graph_filter ()) ;
  dialog#add_filter (all_files ()) ;
  let f = match dialog#run () with
    | `OPEN ->default "<none>" dialog#filename 
    | `DELETE_EVENT | `CANCEL -> "<none>"
  in
  dialog#destroy ();
  f

let open_graph()  =
  let fichier = ask_for_file window in
  if fichier <> "<none>"
  then 
    begin 
      load_graph fichier;
      reset_table_and_canvas ();
      let turtle = make_origine_turtle () in
      draw turtle canvas_root;
      set_canvas_event ()
    end
      
let new_graph () =
  graph := G.create ();
  Model.reset();
  reset_table_and_canvas ()
      
let menu_files = 
  [
    `I ("_New Graph",  new_graph);
    `I ("_Open Graph", open_graph);
    `I ("_Save Graph", print "todo save graph");
    `I ("Save Graph _As ...", print "todo save graph as...");
    `S;
    `I ("_Quit", GMain.Main.quit )
  ]
  
let menu = 
  create_menu "File" menu_bar

let _ = GToolbox.build_menu menu ~entries:menu_files 



let tortue = make_origine_turtle ()

let () = canvas#set_scroll_region 0. 0. w h 

let () = window#show ()
let _ = 
  reset_table_and_canvas ();
  draw tortue canvas_root;
  set_canvas_event ()


let () = GMain.Main.main ()

