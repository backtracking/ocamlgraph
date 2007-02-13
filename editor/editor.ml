
open Format

let _ = GMain.Main.init ()


let print msg () =
  print_endline msg;
  flush stdout


let create_menu label menubar =
  let item = GMenu.menu_item ~label ~packing:menubar#append () in
  GMenu.menu ~packing:item#set_submenu ()


let main () =
  
  (* Initialisation du fichier de graph *)

  if Sys.argv.(1) = "--help" then
    begin
      printf "usage: %s file.gml   # browse graph of file.gml@." Sys.argv.(0) ;
      printf "@." ;
      exit 0
    end ;
(*
  if Array.length Sys.argv > 2 then
    eprintf "%s: ignoring trailing arguments@." Sys.argv.(0) ;
  *)




  (* la Fenetre principale *)
  let window =
    GWindow.window ~border_width: 10 ~title:"Edit'OcamlGraph" () in
  let _ =
    window#connect#destroy~callback:GMain.Main.quit in

 
  (* une Verticale Box  pour contenir le menu de la fenetre principale *)
  let v_box =
    GPack.vbox ~homogeneous:false ~spacing:30  ~packing:window#add () in
  
  (* la barre de Menu ajoutée dans la V_box *)
  let menu_bar =
    GMenu.menu_bar ~packing:v_box#pack () in

  (* le menu file : la description puis l'ajout au menu_bar *)
  let menu_files = 
    [
      `I ("_New Graph", print "todo new graph");
      `I ("_Open Graph", print "todo open graph");
      `I ("_Save Graph", print "todo save graph");
      `I ("Save Graph _As ...", print "todo save graph as...");
      `S;
      `I ("_Quit", GMain.Main.quit )
    ]
  
  and menu = 
    create_menu "File" menu_bar in
  
  GToolbox.build_menu menu ~entries:menu_files ;



  (* la zone d'affichage du graph, le canvas *)
  let canvas = 
    GnoCanvas.canvas ~width:500 ~height:500 ~packing:v_box#add () 
  in

  
  let module GT = Gtree.Make(DirTree) in
  let _ =  GT.show_tree canvas (DirTree.from_dir "" Sys.argv.(1)) (canvas#width) (canvas#height)
  in

  (* l'affichage de la fenetre principale *)

  
  window#show ();
  GMain.Main.main ()

let _ = main ()
