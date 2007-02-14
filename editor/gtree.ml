module type TREE = sig
  type t
  type label
  val children : t -> t list
  val label : t -> label
  val string_of_label : label -> string
end

module type GTREE = sig
  type t
  type label
  val children : t -> t list
  val label : t -> label
  val string_of_label : label -> string

  val show_tree : #GnoCanvas.group -> t -> int -> int -> unit
end



module Make(T : TREE) = struct
  type t = T.t
  type label = T.label
  let children = T.children
  let label = T.label
  let string_of_label = T.string_of_label

  module HT = Htree.Make(T)

  let show_tree canvas t width height =

    let current_x = ref 0 in
    let current_y = ref 0 in

    let moveto zx zy =
      current_x := zx; current_y := zy;
    in
    let gtk_coord x y = float x -. 300., 500. -. float y in

    let lineto zx zy =
      (*Format.eprintf "lineto %d %d@." zx zy;*)
      let cx,cy = gtk_coord !current_x !current_y in
      let fx,fy = gtk_coord zx zy in
      let p = [|cx; cy; fx; fy|] in
      moveto zx zy;
      GnoCanvas.line canvas ~props:[ `POINTS p; `FILL_COLOR "yellow" ;`WIDTH_PIXELS 1]  ;
      ()
    in



    let rlimit = 0.98
    and xzoom = float(width)/.2.0
    and yzoom = float(height)/.2.0 in

    let curveto =
      let rec bezier_rec x0 y0 x1 y1 x2 y2 x3 y3 =
	let dx01 = x1 -. x0
	and dy01 = y1 -. y0
	and dx23 = x3 -. x2
	and dy23 = y3 -. y2
	and dx03 = x3 -. x0
	and dy03 = y3 -. y0 in
	let a1 = abs_float (dx01*.dy03 -. dy01*.dx03)
	and a2 = abs_float (dx23*.dy03 -. dy23*.dx03)
	and d = sqrt(dx03*.dx03 +. dy03*.dy03) in
	let amax = max 1.0 (d/.2.0) in
	if a1 < amax && a2 < amax then
	  lineto (truncate x3) (truncate y3)
	else
	  begin
	    let x01 = (x0 +. x1)/.2.0
	    and y01 = (y0 +. y1)/.2.0
	    and x12 = (x1 +. x2)/.2.0
	    and y12 = (y1 +. y2)/.2.0
	    and x23 = (x2 +. x3)/.2.0
	    and y23 = (y2 +. y3)/.2.0 in
	    let x012 = (x01 +. x12)/.2.0
	    and y012 = (y01 +. y12)/.2.0
	    and x123 = (x12 +. x23)/.2.0
	    and y123 = (y12 +. y23)/.2.0 in
	    let x0123 = (x012 +. x123)/.2.0
	    and y0123 = (y012 +. y123)/.2.0 in
	    bezier_rec x0 y0 x01 y01 x012 y012 x0123 y0123 ;
	    bezier_rec x0123 y0123 x123 y123 x23 y23 x3 y3
	  end in
      fun x1 y1 x2 y2 x3 y3 ->
	bezier_rec
	  (float !current_x) (float !current_y)
	  (float x1) (float y1)
	  (float x2) (float y2)
	  (float x3) (float y3) 
    in

    let common_moveto (zx, zy) =
      (*Format.printf "moveto%d   ma coord est zx: %f  zy: %f\n"!emoveto zx zy;*)
      let x = truncate(zx*.xzoom +. xzoom)
      and y = truncate(zy*.yzoom +. yzoom) in
      moveto x y

    and common_lineto (zx, zy) =
      let x = truncate(zx*.xzoom +. xzoom)
      and y = truncate(zy*.yzoom +. yzoom) in
      lineto x y in

    let common_curveto (zx1, zy1) (zx2, zy2) (zx3, zy3) =
      let x1 = truncate (zx1*.xzoom +. xzoom)
      and x2 = truncate (zx2*.xzoom +. xzoom)
      and x3 = truncate (zx3*.xzoom +. xzoom)
      and y1 = truncate (zy1*.yzoom +. yzoom)
      and y2 = truncate (zy2*.yzoom +. yzoom)
      and y3 = truncate (zy3*.yzoom +. yzoom) in
      curveto x1 y1 x2 y2 x3 y3
    in

    let drag_label item ev = 
      begin match ev with
	| `ENTER_NOTIFY _ ->
	    item#set [ `FILL_COLOR "red" ]
	| `LEAVE_NOTIFY ev ->
	    let state = GdkEvent.Crossing.state ev in
	    if not (Gdk.Convert.test_modifier `BUTTON1 state)
	    then item#set [ `FILL_COLOR "blue" ; ]
	| `BUTTON_PRESS ev ->
	    let curs = Gdk.Cursor.create `FLEUR in
	    item#grab [`POINTER_MOTION; `BUTTON_RELEASE] curs 
	      (GdkEvent.Button.time ev)
	| `BUTTON_RELEASE ev ->
	    item#ungrab (GdkEvent.Button.time ev)
	| _ -> ()
      end ;
      false 
    in

    
    let draw_label lab (zx, zy) facteur_reduction =
      let x = truncate (zx*.xzoom +. xzoom)
      and y = truncate (zy*.yzoom +. yzoom) in
      let name = string_of_label lab in
      let (w,h) = (10,6) in
      let x0 = x - w/2
      and y0 = y - h/2 in
      moveto x0 y0;
      let label =
	let fx,fy = gtk_coord x0 y0 in
	GnoCanvas.text	~props:[ `X fx ; `Y fy; `TEXT name;  `FILL_COLOR "blue"] canvas in   
(*	GnoCanvas.rect 
	~props:[ `X1 (x0 -. 2.) ; `Y1 (y0 +. h +. 2.) ;
		 `X2 (x0 +. w +. 4.) ; `Y2 (y0 -. 1.) ;
		 `FILL_COLOR "blue" ; `OUTLINE_COLOR "black" ; `WIDTH_PIXELS 0 ] canvas in      
*)
      let sigs = label#connect in
      sigs#event (drag_label label) ;
      label;
      () 
    in

      
    let gtk_draw_init_edge_pass () = ()
      
    and gtk_draw_init_label_pass () = () in
    
    let draw_drv = {
		     HT.rlimit = rlimit ;
		     HT.moveto = common_moveto ;
		     HT.lineto = common_lineto ;
		     HT.curveto = common_curveto ;
		     HT.draw_label = draw_label ;
		     HT.init_edge_pass = gtk_draw_init_edge_pass ;
		     HT.init_label_pass = gtk_draw_init_label_pass ;
		     HT.finalize = (fun () -> ())
		   } 
    in
      
    
    let xy2c x y =
      let zx = (float(x) -. xzoom)/.xzoom
      and zy = (float(y) -. yzoom)/.yzoom in
      let zn = sqrt(zx*.zx +. zy*.zy) in
      if zn > rlimit then
	(rlimit*.zx/.zn, rlimit*.zy/.zn)
      else
	(zx, zy)
	  
    and draw_linear_tree = HT.draw_linear_tree draw_drv
    and draw_curved_tree = HT.draw_curved_tree draw_drv

    in
    draw_curved_tree t (0.0,0.0) 0.0
     
end

