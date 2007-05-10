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

  let rows = Hashtbl.create 97

  let add_vertex v =
    let row = model#append () in
    model#set ~row ~column:name (string_of_label v);
    model#set ~row ~column:vertex v;
    Hashtbl.add rows v row;
    row

  let add_edge_1 row_v w =
    let row = model#append ~parent:row_v () in
    model#set ~row ~column:name (string_of_label w)

  let reset () =
    Hashtbl.clear rows;
    model#clear ();
    G.iter_vertex
      (fun v -> 
	 let row = add_vertex v in
	 G.iter_succ (add_edge_1 row) !graph v)
      !graph

  let add_edge v w =
    let row_v = Hashtbl.find rows v in
    add_edge_1 row_v w;
    if not G.is_directed then 
      let row_w = Hashtbl.find rows w in
      add_edge_1 row_w v

end

let () = Model.reset ()
let model = ref Model.model

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



(* current root used for drawing *)
let root = ref (choose_root ())


let load_graph f =
  Ed_graph.load_graph f;
  Model.reset ();
  root := choose_root ()



(* refresh rate *)
let refresh = ref 0


(* graph drawing *)
let draw tortue canvas =
  canvas#hide();
  Ed_draw.draw_graph !root tortue;
  Ed_display.draw_graph !root canvas;
  canvas#show()


(* selected node List *)
let vertex_selection = []
let is_selected x =  List.mem x vertex_selection


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


(* event for each vertex of canvas *)
let vertex_event noeud item ev =
  begin match ev with
    | `ENTER_NOTIFY _ ->
	if  not (is_selected noeud)
	then begin	
	  item#set [ `FILL_COLOR "steelblue" ];
	  color_change_intern_edge "blue" noeud ; 
	  color_change_successor_edge "blue" noeud 
	end;
    | `LEAVE_NOTIFY ev ->
	if  not (is_selected noeud)
	then begin	
	  let state = GdkEvent.Crossing.state ev in
	  if not (Gdk.Convert.test_modifier `BUTTON1 state)
	  then item#set [ `FILL_COLOR "grey" ; ];
	  color_change_intern_edge "SlateGrey" noeud ;
	  color_change_successor_edge "black" noeud;
	 (* begin match !select with
	    | None -> ()
	    | Some (n,i) -> begin	
		color_change_intern_edge "red" n ;
		color_change_direct_edge "red" n
	      end 
	  end;
	 *)
	end 
    | `BUTTON_RELEASE ev ->
	item#parent#ungrab (GdkEvent.Button.time ev)
    | `MOTION_NOTIFY ev ->
	incr refresh;
	let state = GdkEvent.Motion.state ev in
	if Gdk.Convert.test_modifier `BUTTON1 state && !refresh mod 10 = 0 then 
	  begin
	    let curs = Gdk.Cursor.create `FLEUR in
	    item#parent#grab [`POINTER_MOTION; `BUTTON_RELEASE] curs (GdkEvent.Button.time ev);
	    let tmp = !origine in	    
	    let turtle = motion_turtle item ev in
	    if hspace_dist_sqr turtle <= rlimit_sqr
	    then begin
	      draw turtle canvas_root;
	      (*if !refresh mod 15 = 0 then*)
		canvas_root#canvas#update_now ()
	    end else 
	      origine := tmp
	  end
	    (*   | `TWO_BUTTON_PRESS ev->
      if (GdkEvent.Button.button ev) = 1
		 then selectionner_noeud noeud item;
      | `BUTTON_PRESS ev ->
      if (GdkEvent.Button.button ev) = 1
        then deselectionner_noeud noeud item ;
	if (GdkEvent.Button.button ev) = 3
        then
	  begin
            let loc_menu = GMenu.menu () in
            let factory =
              new GMenu.factory loc_menu in
            ignore (factory#add_item "  Ajouter un successeur" ~callback: (ajout_successeur noeud));
	    begin match !select with
	      | None -> ()
	      | Some (n,_) -> 
		  if not(V.equal n noeud)
		  then begin
		    ignore (factory#add_item "  Ajouter une arrête" ~callback: (ajout_arrete n noeud));	    
		  end 
	    end;

            loc_menu#popup
              ~button:3
              ~time:(GdkEvent.Button.time ev);
          end
 *)
    | _ ->
	()
  end;
  true

let set_canvas_event ()=
(* vertex event *)
  G.iter_vertex
    (fun v -> 
      let l = G.V.label v in
      if l.visible = Visible then 
	let item = H.find ellipses v in
	ignore (item#parent#connect#event (vertex_event v item) ) 
    )
    !graph


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


let treeview = GTree.view ~model:!model ~packing:sw#add ()
let () = treeview#set_rules_hint true
let () = treeview#selection#set_mode `MULTIPLE
let _ = add_columns ~view:treeview ~model:!model
(*let _ = treeview#misc#connect#realize ~callback:treeview#expand_all*)

(* reset *)

let reset_table_and_canvas () =
  let l =  canvas_root#get_items in
  List.iter (fun v -> trace v#destroy ()) l;
  H2.clear intern_edges;
  H2.clear successor_edges;
  reset_display canvas_root;
  origine := start_point



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
  model := Model.model;
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

