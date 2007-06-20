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

  let remove_vertex vertex = 
    let row = find_row vertex in
    model#remove row

  let add_edge_1 row_v w =
    let row = model#append ~parent:row_v () in
    model#set ~row ~column:name (string_of_label w)


  let add_edge v w =
    let row_v = find_row v in
    add_edge_1 row_v w;
    if not G.is_directed then 
      let row_w = find_row w in
      add_edge_1 row_w v

  let remove_edge v w = ()
   
  let reset () =
    H.clear rows;
    model#clear ();
    G.iter_vertex
      (fun v -> 
	 let row = add_vertex v in
	 G.iter_succ (add_edge_1 row) !graph v)
      !graph


 (* let remove_edge vertex =*)

   
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
  GnoCanvas.canvas ~aa:!aa ~width:(truncate w) ~height:(truncate h) 
    ~packing:h_box#add () 

let canvas_root = canvas#root 



(* unit circle and graph root *)
let canvas_root =
  let circle_group = GnoCanvas.group ~x:300.0 ~y:300.0 canvas_root in
  circle_group#lower_to_bottom ();
  let w2 = 2. in
  let circle =
    GnoCanvas.ellipse  ~props:[ `X1 (-.w/.2. +.w2); `Y1 (-.h/.2. +.w2); 
				`X2  (w/.2. -.w2) ; `Y2 ( h/.2. -.w2) ;
 				`FILL_COLOR color_circle ; `OUTLINE_COLOR "black" ; 
				`WIDTH_PIXELS (truncate w2) ] circle_group 
  in
  circle_group#lower_to_bottom ();
  circle#show();
  let graph_root = GnoCanvas.group ~x:(-.300.0) ~y:(-.300.0) circle_group in
  graph_root#raise_to_top ();
  graph_root



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
  Ed_draw.draw_graph !root tortue;
  Ed_display.draw_graph !root canvas;
  if do_refresh () then
    canvas_root#canvas#update_now ()


let refresh_draw () =
  refresh := 0;    
  let tor = make_turtle !origine 0.0 in
  draw tor canvas_root
    
let refresh_display () =
  Ed_display.draw_graph !root canvas_root



let root_change vertex ()= 
  root := vertex; 
  origine := start_point;
  let turtle = make_turtle_origine () in
  draw turtle canvas_root




let node_selection ~(model : GTree.tree_store) path =
  let row = model#get_iter path in
  let vertex = model#get ~row ~column: Model.vertex in
  root_change vertex ()





(* usual function ref, for vertex event *)
let set_vertex_event_fun = ref (fun _ -> ())

(* type to select nature of modification *)
type modification = Add | Remove

(* add a vertex with no successor *)
let add_node () =
  let window = GWindow.window 
    ~title: "Choose vertex label" 
    ~width: 300 
    ~height: 50 () in
  let vbox = GPack.vbox 
    ~packing: window#add () in
  let entry = GEdit.entry 
    ~max_length: 50 
    ~packing: vbox#add () in
  entry#set_text "Label";
  entry#select_region 
    ~start:0 
    ~stop:entry#text_length;
  (*two check buttons*)
  let hbox = GPack.hbox 
    ~packing: vbox#add () in
  let is_in_selection = ref false in
  let in_selection = GButton.check_button  
    ~label: "Add to selection" 
    ~active:!is_in_selection
    ~packing: hbox#add () in
  ignore (in_selection#connect#toggled 
	    ~callback:(fun () ->is_in_selection := in_selection#active ));
  let is_as_root = ref ((G.nb_vertex !graph)=0) in
  let as_root = GButton.check_button 
    ~label:"Choose as root" 
    ~active:!is_as_root 
    ~packing:hbox#add () in
  ignore (as_root#connect#toggled
	    ~callback:(fun () ->is_as_root := as_root#active ));
  window#show ();
  (*entry's callback*)
  ignore( entry#connect#activate 
	    ~callback: (fun () ->
			  let text = entry#text in
			  window#destroy ();
			  (* new vertex *)
			  let vertex = G.V.create (make_node_info text)  in
			  G.add_vertex !graph  vertex ;
			  ignore (Model.add_vertex vertex);
			  Ed_display.add_node canvas_root vertex;
			  !set_vertex_event_fun vertex;
			  if !is_as_root  then root_change vertex () ;
			  if !is_in_selection then update_vertex vertex Select;
			  let  tor = make_turtle !origine 0.0 in
			  draw tor canvas_root))






(* add an edge between n1 and n2 , add link in column and re-draw *)
let add_edge n1 n2 ()= 
  if not (edge n1 n2)
  then begin
    G.add_edge_e !graph (G.E.create n1 (make_edge_info ()) n2);
    Model.add_edge n1 n2;
    let tor = make_turtle !origine 0.0 in
    draw tor canvas_root;
  end


let add_edge_no_refresh n1 n2 ()= 
  if not (edge n1 n2)
  then begin
    G.add_edge_e !graph (G.E.create n1 (make_edge_info ()) n2);
    Model.add_edge n1 n2
  end

(* remove an edge between n1 and n2 , add un-link in column and re-draw *)
let remove_edge n1 n2 ()= 
  if (edge n1 n2)
  then begin
    G.remove_edge !graph  n1 n2;
    Model.remove_edge n1 n2;
    begin
      try
	let _,n = H2.find intern_edges (n1,n2) in
	n#destroy ();
	H2.remove intern_edges (n1,n2) 
      with Not_found ->
	try
	   let _,n = H2.find intern_edges (n2,n1) in
	   n#destroy ();
	   H2.remove intern_edges (n2,n1) 
	with Not_found ->
	  try
	    let n = H2.find successor_edges (n1,n2) in
	    n#destroy ();
	    H2.remove successor_edges (n1,n2) 
	  with Not_found ->
	    try
	      let n = H2.find successor_edges (n2,n1) in
	      n#destroy ();
	      H2.remove successor_edges (n2,n1) 
	    with Not_found -> ()
    end;
    let tor = make_turtle !origine 0.0 in
    draw tor canvas_root;
  end


let remove_edge_no_refresh n1 n2 ()= 
  if (edge n1 n2)
  then begin
    G.remove_edge !graph n1 n2;
    Model.remove_edge n1 n2
  end



(* add successor node to selected node *)
let add_successor node () =
  let window = GWindow.window 
    ~title: "Choose label name" 
    ~width: 300 
    ~height: 50 
    () in
  let vbox = GPack.vbox 
    ~packing: window#add 
    () in
  let entry = GEdit.entry 
    ~max_length: 50 
    ~packing: vbox#add 
    () in
  entry#set_text "Label";
  entry#select_region 
    ~start:0 
    ~stop:entry#text_length;
  window#show ();
  ignore (entry#connect#activate 
	    ~callback:(fun () ->
			 let text = entry#text in
			 window#destroy ();
			 (* new vertex *)
			 let vertex = G.V.create (make_node_info text)  in
			 G.add_vertex !graph  vertex ;
			 ignore (Model.add_vertex vertex);
			 Ed_display.add_node canvas_root vertex;
			 !set_vertex_event_fun vertex;
			 (* new edge *)
			 G.add_edge_e !graph (G.E.create node (make_edge_info()) vertex);
			 Model.add_edge node vertex;
			 (* redraw *)
			 let tor = make_turtle !origine 0.0 in
			 draw tor canvas_root
		      )
	 )

let remove_vertex vertex () =
  G.iter_succ
    (fun w ->
       try
	 let _,n = H2.find intern_edges (vertex,w) in
	 n#destroy ();
	 H2.remove intern_edges (vertex,w) 
       with Not_found ->
	 try
	   let _,n = H2.find intern_edges (w,vertex) in
	   n#destroy ();	    
	   H2.remove intern_edges (w,vertex) 
	 with Not_found ->
	   try
	     let n = H2.find successor_edges (vertex,w) in
	     n#destroy ();
	     H2.remove successor_edges (vertex,w) 
	   with Not_found ->
	     try
	       let n = H2.find successor_edges (w,vertex) in
	       n#destroy ();
	       H2.remove successor_edges (w,vertex) 
	     with Not_found ->
	       ()
    )
    !graph vertex;
  let (n,_,_) =  H.find nodes vertex in
   n#destroy ();
  H.remove nodes vertex;
  G.remove_vertex !graph vertex;
  ignore (Model.remove_vertex vertex);
  if (G.V.equal !root vertex) && not (G.is_empty !graph)
  then root := choose_root();
  refresh_draw ()
    


let sub_edge_to modif_type vertex list =
  let ll = List.length list in
  let nb_sub_menu = (ll - 1)/10 + 1 in
  let nb_edge =  ll / nb_sub_menu -1 in
  let menu = new GMenu.factory (GMenu.menu()) in
  let sub_menu =ref (new GMenu.factory (GMenu.menu())) in
  let add_edge  vertex v2 =
    if not (G.V.equal v2 vertex)
    then begin
      match modif_type with
	| Add ->ignore((!sub_menu)#add_item (string_of_label v2) 
	       ~callback:( add_edge v2 vertex));
	| Remove -> if (edge v2 vertex) 
	  then ignore((!sub_menu)#add_item (string_of_label v2) 
	       ~callback:(remove_edge v2 vertex));
    end;
  in
  let rec make_sub_menu vertex list nb =
    match list with
      | [] -> ()
      | v::list ->
	  match nb with
	    | 0 -> 
		begin
		  sub_menu :=new GMenu.factory (GMenu.menu()) ;
		  add_edge vertex v;
		  let string = string_of_label v in
		  ignore (menu#add_item (String.sub string 0 (min (String.length string) 3)^"...") 
			    ~submenu: !sub_menu#menu);
		  make_sub_menu vertex list (nb+1);
		end
	    | n when n= nb_edge-> 
		begin
		  add_edge vertex v;
		  make_sub_menu vertex list 0
		end
	    | _ ->
		begin
		  add_edge vertex v;
		  make_sub_menu vertex list (nb+1)
		end
  in
  if ll > 10 
  then begin
    make_sub_menu vertex list 0;
    menu
  end
  else begin
    
    let rec make_sub_bis list =
      match list with
	| [] -> ();
	| v::list ->add_edge vertex v; make_sub_bis list
    in
    make_sub_bis list;
    !sub_menu
  end
    

    
let edge_to modif_type vertex list =
  (* add an edge between current vertex and one of selected vertex*)
  sub_edge_to modif_type vertex list



let all_edges (edge_menu :#GMenu.menu GMenu.factory) vertex list =
  (*add all edges as possible from current vertex to selected vertices*)
  begin
    let add_all_edge vertex list () = 
      List.iter (fun v -> if not (G.V.equal v vertex)
		 then add_edge_no_refresh v vertex()
		)
	list ;
      refresh := 0;    
      let tor = make_turtle !origine 0.0 in
      draw tor canvas_root
    in
    ignore (edge_menu#add_item "Add all edges" ~callback:( add_all_edge vertex list))
  end



let contextual_menu vertex ev =

  let menu = new GMenu.factory (GMenu.menu ()) in
  (* change root*)
  ignore (menu#add_item "As root" ~callback:(root_change vertex));

  (*vertex menu*)
  let vertex_menu = new GMenu.factory (GMenu.menu ()) in
  begin
    (* successor *)
    ignore (vertex_menu#add_item "Add successor" ~callback:(add_successor vertex));
    ignore (vertex_menu#add_separator ());
    (* remove vertex *)
    ignore(vertex_menu#add_item "Remove vertex" ~callback:(remove_vertex vertex));
  end;
  ignore(menu#add_item "Vertex ops" ~submenu: vertex_menu#menu);

  (*edge menu*)
  begin
    let sel_list = selected_list () in
    match sel_list with
      | [] -> ()
      | list ->
	  let ll =List.length list in
	  let isel = is_selected vertex in
	  begin
	    if isel && ll=1 then ()
	    else
	      begin
		let edge_menu = new GMenu.factory (GMenu.menu ()) in
		if isel && ll=2 ||
		  not isel && ll=1
		then begin
		  ignore (edge_menu#add_item "Add edge with" ~submenu: (edge_to Add vertex list)#menu);
		  ignore (edge_menu#add_separator ());
		  ignore (edge_menu#add_item "Remove edge with" ~submenu: (edge_to Remove vertex list)#menu);
		end;
		if isel && ll>2 ||
		  not isel && ll>1
		then  begin
		  ignore (edge_menu#add_item "Add edge with" ~submenu: (edge_to Add vertex list)#menu);
		  all_edges edge_menu vertex list;
		  ignore (edge_menu#add_separator ());
		  ignore (edge_menu#add_item "Remove edge with" ~submenu: (edge_to Remove vertex list)#menu);
		end;
		ignore(menu#add_item "Edge ops" ~submenu: edge_menu#menu);
	      end;
	  end;
  end;	  
  menu#menu#popup ~button:3 ~time:(GdkEvent.Button.time ev)
	    







(* unit circle callback *)
let circle_event ev =
  begin match ev with
    | `BUTTON_PRESS ev ->
 	if (GdkEvent.Button.button ev) = 3
        then
	  begin
	    let menu = new GMenu.factory (GMenu.menu ()) in
	    ignore (menu#add_item " Add node" ~callback:(add_node));
	    menu#menu#popup
	      ~button:3
	      ~time:(GdkEvent.Button.time ev)
          end
    | _ ->()
  end;
  true



(* event for each vertex of canvas *)
let vertex_event vertex item ellispe ev =

 (* let vertex_info = G.V.label vertex in*)
  begin match ev with
    | `ENTER_NOTIFY _ ->
	item#grab_focus ();
	 update_vertex vertex Focus;
	 refresh_display ()

    | `LEAVE_NOTIFY ev ->
	if not (Gdk.Convert.test_modifier `BUTTON1 (GdkEvent.Crossing.state ev))
	then begin
	  update_vertex vertex Unfocus;
	  refresh_display ()
	end
	  
    | `BUTTON_RELEASE ev ->
	ellispe#parent#ungrab (GdkEvent.Button.time ev);
	
    | `MOTION_NOTIFY ev ->
	incr refresh;
	let state = GdkEvent.Motion.state ev in
	if Gdk.Convert.test_modifier `BUTTON1 state  then 
	  begin
	    let curs = Gdk.Cursor.create `FLEUR in
	    ellispe#parent#grab [`POINTER_MOTION; `BUTTON_RELEASE] 
	      curs (GdkEvent.Button.time ev);
	    if do_refresh ()
	    then begin
	      let old_origin = !origine in
	      let turtle = motion_turtle ellispe ev in
	      if hspace_dist_sqr turtle <= rlimit_sqr then begin
		draw turtle canvas_root
	      end else begin
		origine := old_origin;
		let turtle = { turtle with pos = old_origin } in
		draw turtle canvas_root
	      end
	    end
	  end

    | `BUTTON_PRESS ev ->
 	if (GdkEvent.Button.button ev) = 3
        then
	  begin
	    contextual_menu vertex ev
          end
	    
    | `TWO_BUTTON_PRESS ev->
      if (GdkEvent.Button.button ev) = 1
      then begin
	if (Gdk.Convert.test_modifier `CONTROL (GdkEvent.Button.state ev))
	then begin
	  if ( !nb_selected =0)
	  then begin
	    select_all ();
	    update_vertex vertex Focus
	  end
	  else begin
	    unselect_all ();
	    update_vertex vertex Focus
	  end
	end
	else begin
	  if (is_selected vertex)
	  then update_vertex vertex Unselect
	  else update_vertex vertex Select;
	end;
	refresh_draw ();
      end;  

    | _ ->
	()
  end;
  true


let set_vertex_event vertex =
  let item,ell,_ = H.find nodes vertex in
  ignore (item#connect#event (vertex_event vertex item ell))

let () = set_vertex_event_fun := set_vertex_event

let set_canvas_event () =
  (* circle event *)
  ignore(canvas_root#parent#connect#event (circle_event));
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
  nb_selected:=0


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
      let turtle = make_turtle_origine () in
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



let tortue = make_turtle_origine ()

let () = canvas#set_scroll_region 0. 0. w h 

let () = window#show ()
let _ = 
  reset_table_and_canvas ();
  draw tortue canvas_root;
  set_canvas_event ()


let () = GMain.Main.main ()

