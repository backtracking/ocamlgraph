
module G = Graph.Pack.Graph

let _ = GMain.Main.init ()

let file = Sys.argv.(1)
let g = G.parse_gml_file file

let main () =
  (* toplevel window *)
  let window =GWindow.window ~border_width: 10 ~title:"Graph Editor" () in
  let _ = window#connect#destroy ~callback:(fun () -> exit 0) in
  let vbox = GPack.vbox ~homogeneous:false ~packing:window#add () in
  (* Menu *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "_File" in
  let file_factory = new GMenu.factory file_menu ~accel_group in
  let _ = 
    file_factory#add_image_item ~label:"_Load graph"
      ~callback:(fun () -> print_endline "load_graph(todo)"; (* TODO *) ()) 
      ~key:GdkKeysyms._L () 
  in
  let _ = file_factory#add_separator () in
  let _ = 
    file_factory#add_image_item ~key:GdkKeysyms._Q ~label:"_Quit" 
      ~callback:(fun () -> exit 0) () 
  in
  (* canvas *)
  let canvas = 
    GnoCanvas.canvas ~width:500 ~height:350 ~packing:vbox#add () 
  in
  let root = canvas#root in
  let p = [| 0.; 0.; 100.; 100.; |] in
  let big_arrow = GnoCanvas.line root
      ~props:[ `POINTS p ; `FILL_COLOR "mediumseagreen" ;
	       `LAST_ARROWHEAD true ] 
  in
  (* ... *)
  (* show all and enter event loop *)
  window#add_accel_group accel_group;
  window#show ();
  GMain.Main.main ()

let _ = Printexc.print main()
