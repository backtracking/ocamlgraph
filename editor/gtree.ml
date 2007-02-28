

(*
module T =
struct
  type t = DirTree.t
  type label = DirTree.t
  let id = DirTree.id 
  let edges = Hashtbl.create 97
  let children t = 
    let l = DirTree.children t in
    List.iter (fun i -> Hashtbl.replace edges (DirTree.id t, DirTree.id i) ()) l;
    l
  let label x = x
  let string_of_label x = DirTree.string_of_label (DirTree.label x)
 
  let root = DirTree.from_dir "" Sys.argv.(1)
end
*)


type drag_box = 
    { db_noeud : GnoCanvas.ellipse;
      mutable db_x : float;
      mutable db_y : float;
      db_w : float;
      db_h : float;
      mutable db_viewable : bool;
    }
      
let drag_boxes = Hashtbl.create 97

module T =
struct
  open Graph.Pack.Graph

  let g = parse_gml_file Sys.argv.(1)

  exception Choose of V.t
  let root = 
    try
      iter_vertex (fun v -> raise (Choose v)) g;
      Format.eprintf "empty graph@."; exit 0
    with Choose v ->
      v

  type t = V.t
  type label = V.t

  module H = Hashtbl.Make(V)
  let ids = H.create 97
  let id = 
    let r = ref 0 in
    fun v -> try H.find ids v with Not_found -> incr r; H.add ids v !r; !r

  let edges = Hashtbl.create 97

  let order_children l =
    l (*TODO*)

  let children v = 
    let l = succ g v in
    List.iter (fun i -> Hashtbl.replace edges (id v, id i) ()) l;
    let l = 
      List.filter 
	(fun w -> 
	   try not (Hashtbl.find drag_boxes (id w)).db_viewable
	   with Not_found -> true) 
	l
    in
    order_children l

  let label x = x
  let string_of_label x = string_of_int (V.label x)
end

module HT = Htree.Make(T)


let lines = Hashtbl.create 97

let show_tree canvas t width height =
  let rlimit = 0.98 in
  let xzoom = float(width)/.2.0
  and yzoom = float(height)/.2.0 in
  let origin = ref (-0.5,0.0) in
  let xy2gtk x y = float x -. 300., float(height)/.2. -. float y +. 50. in
  let gtk2xy gx gy = truncate (gx +. 300.), truncate ((float height)/.2. +. 50. -. gy) in
  let xy2c (x, y) =
    let zx = (float(x) -. xzoom)/.xzoom
    and zy = (float(y) -. yzoom)/.yzoom in
    let zn = sqrt(zx*.zx +. zy*.zy) in
    if zn > rlimit then
      (rlimit*.zx/.zn, rlimit*.zy/.zn)
    else
      (zx, zy)
  in
  let draw_edges () =
    let draw_edge (i,j) () = 
      try
	let dbi = Hashtbl.find drag_boxes i in
	let dbj = Hashtbl.find drag_boxes j in
	let l =
	  try
	    Hashtbl.find lines (i,j)
	  with Not_found-> 
	    let l = GnoCanvas.line canvas ~props:[ `FILL_COLOR "black" ;`WIDTH_PIXELS 1; `SMOOTH true]  in
	    Hashtbl.add lines (i,j) l;
	    l
	in
	let p = [| dbi.db_x; dbi.db_y; dbj.db_x; dbj.db_y |] in
	l#set [`POINTS p];
	l#lower_to_bottom ()

      with Not_found -> 
	  try
	    let l= Hashtbl.find lines (i,j) in
	    l#destroy();
	    Hashtbl.remove lines (i,j)
	  with Not_found ->
	    ()

    in
 
    Hashtbl.iter draw_edge T.edges
  in
  let rec draw_label lab (zx,zy) facteur_reduction = 
    let x = truncate (zx*.xzoom +. xzoom)
    and y = truncate (zy*.yzoom +. yzoom) in
    let name = T.string_of_label lab in
    let (w,h) = (40,15) in
    let x0 = x - w/2
    and y0 = y - h/2 in
    let fx,fy = xy2gtk x0 y0 in
    try
      let db = Hashtbl.find drag_boxes (T.id lab) in
      let bounds =  db.db_noeud#parent#get_bounds in 
      db.db_x <- fx;
      db.db_y <- fy;
      db.db_viewable <- true;
      db.db_noeud#parent#set [ `X fx; `Y fy; ];
      db.db_noeud#parent#move ~x:zx ~y:zy;


    with Not_found ->
      let noeud = GnoCanvas.group ~x:fx ~y:fy  canvas in
      let ellipse = GnoCanvas.ellipse 
	~props:[ `X1  ( float (-w/2)); `Y1 (float (-h/2)); `X2  (float (w/2)) ; `Y2 ( float (h/2)) ;
		   `FILL_COLOR "grey" ; `OUTLINE_COLOR "black" ; `WIDTH_PIXELS 0 ] noeud
      in
      let _ = GnoCanvas.text ~props:[`X 0.0; `Y 0.0 ; `TEXT name;  `FILL_COLOR "blue"] noeud in
      let sigs = noeud#connect in
      let db = { db_noeud = ellipse; db_x = fx; db_y = fy; db_w = float w; db_h = float h; db_viewable = true } in
      let _ = sigs#event (drag_label db) in
      Hashtbl.add drag_boxes (T.id lab) db;

(*      let rect = 
	 let _ = GnoCanvas.text ~props:[ `X (fx-.20.) ; `Y (fy+.30.); `TEXT name;  `FILL_COLOR "blue"]canvas in
	GnoCanvas.ellipse 
	  ~props:[ `X1  (fx-.30.); `Y1 (fy-.20.); `X2 (fx +. float w) ; `Y2 (fy +. float h) ;
		   `FILL_COLOR "grey" ; `OUTLINE_COLOR "black" ; `WIDTH_PIXELS 0 ] canvas 
      in
        let db = { db_rect = rect; db_x = fx; db_y = fy; db_w = float w; db_h = float h; db_viewable = true } in
      Hashtbl.add drag_boxes (T.id lab) db;
      let sigs = rect#connect in
      let _ = sigs#event (drag_label db) in
      () 
*)
  and draw_drv = 
    { HT.rlimit = rlimit ;
      HT.moveto = (fun _ -> ());
      HT.lineto = (fun _ -> ());
      HT.curveto = (fun _ _ _ -> ());
      HT.draw_label = draw_label ;
      HT.init_edge_pass = (fun () -> ());
      HT.init_label_pass = (fun () -> ());
      HT.finalize = (fun () -> ())
    } 
  and draw_linear_tree t c f = 
    (* mettre toutes les boites à faux *)
    Hashtbl.iter (fun _ db -> db.db_viewable <- false) drag_boxes;
    HT.draw_linear_tree draw_drv t c f;
    (* détruire toutes les boites restées à faux et les aretes correspondantes *)
    let l = Hashtbl.fold 
      (fun i db acc -> 
	 if not db.db_viewable 
	 then 
	   begin 
	     db.db_noeud#parent#destroy (); 
	     db.db_noeud#destroy (); 

	     i::acc 
	   end 
	 else acc) 
      drag_boxes []
    in
    List.iter (fun i -> Hashtbl.remove drag_boxes i) l;
    draw_edges ()
  and drag_label db ev =
    let item = db.db_noeud in 
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
	  let state = GdkEvent.Motion.state ev in
	  if Gdk.Convert.test_modifier `BUTTON1 state then 
	    begin
	      let curs = Gdk.Cursor.create `FLEUR in
	      item#parent#grab [`POINTER_MOTION; `BUTTON_RELEASE] curs 
		(GdkEvent.Button.time ev);
	      let z1 = xy2c (gtk2xy db.db_x db.db_y) in
	      let mx = GdkEvent.Motion.x ev in
	      let my = GdkEvent.Motion.y ev in
	      let z2 = xy2c (gtk2xy mx my) in
	      item#parent#move ~x:mx ~y:my;
	      item#parent#set [`X mx; `Y my];	      (* inutil ? *)
	      db.db_x <- mx;
	      db.db_y <- my;
	      origin := HT.drag_origin !origin z1 z2;
	      draw_linear_tree t !origin 0.0;
	    end
      | _ ->
	  ()
    end;
    true
  in
  draw_linear_tree t !origin 0.0
