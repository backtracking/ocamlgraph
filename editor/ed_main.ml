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
  | Some (w,_) -> V.equal v w


(* refresh rate *)

let step = ref 0


(* graph drawing *)

let draw tortue canvas =
  canvas#hide();
  Ed_draw.draw_graph !root tortue;
  Ed_display.draw_graph canvas;
  canvas#show()

let () = window#show ()

let () = GMain.Main.main ()

