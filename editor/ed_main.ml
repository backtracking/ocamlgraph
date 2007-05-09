open Format
open Graph

open Ed_hyper
open Ed_graph
open Ed_display

let debug_graphEdGTK = ref false
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
  GWindow.window ~border_width: 10 ~title:"GraphEd" ~position: `CENTER () 

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


(* current selection *)

let select = ref None
let is_selected_node v = match !select with
  | None -> false
  | Some (w(*,_*)) -> G.V.equal v w


(* refresh rate *)

let step = ref 0


(* graph drawing *)

let draw tortue canvas =
  canvas#hide();
  Ed_draw.draw_graph !root tortue;
  Ed_display.draw_graph canvas;
  canvas#show()


(* events *)

let node_selection ~(model : GTree.tree_store) path =
  let row = model#get_iter path in
  let v = model#get ~row ~column: Model.vertex in
  root := v;
  let tortue =
    origine := depart;
    let (x,y) = from_tortue !origine in
    moveto_gtk x y;
    make_turtle !origine 0.0;
  in
  let l = canvas_root#get_items in
  Format.eprintf "il y a %d elements dans le canvas @." (List.length l);
  List.iter (fun v -> v#hide()) l;
  draw tortue canvas_root

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
  H2.clear grey_edges;
  H2.clear black_edges;
  H.clear ellipses;
  origine := depart

(* menu *)

let create_menu label menubar =
  let item = GMenu.menu_item ~label ~packing:menubar#append () in
  GMenu.menu ~packing:item#set_submenu ()
    
let print msg () =
  print_endline msg;
  flush stdout

let open_graph()  =

  let default d = function
    | None -> d
    | Some v -> v
  in
  let all_files () =
    let f = GFile.filter ~name:"All" () in
    f#add_pattern "*" ;
    f
  in
  let graph_filter () = 
    GFile.filter 
      ~name:"Fichier de graphes" 
      ~patterns:[ "*.dot"; "*.gml" ] ()
  in
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
  in
  let fichier = ask_for_file window in
  if fichier <> "<none>"
  then 
    begin 
      load_graph fichier;
      reset_table_and_canvas ();
      let tortue =
	let (x,y) = from_tortue !origine in
	moveto_gtk x y;
	make_turtle !origine 0.0
      in
      draw tortue canvas_root
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



let tortue =
  let (x,y) = from_tortue !origine in
  moveto_gtk x y;
  make_turtle !origine 0.0 

let () = canvas#set_scroll_region 0. 0. w h 

let () = window#show ()
let _ = draw tortue canvas_root
let () = GMain.Main.main ()

