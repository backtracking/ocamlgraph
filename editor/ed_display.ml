
open Format
open Ed_hyper
open Ed_graph

let debug = ref false

let (w,h)= (600.,600.)

let to_tortue(x,y)=
  ((float x*.(2./.w) -. 1.),(1. -. float y *.(2./.h)))
(*  ((float x*.(2./.w) ),(float y *.(2./.h) ))*)

let from_tortue (x,y) =
  let xzoom = (w/.2.)
  and yzoom = (h/.2.) in
  (truncate (x*.xzoom +. xzoom), truncate(yzoom -. y*.yzoom))

let depart = to_tortue (truncate(w/.2.), truncate(h/.2.))

let origine =ref depart

(* GTK *)
let point_courant = ref (0,0)
(*let canvas = graphEdGTK.root *)

let moveto_gtk x y = point_courant := (x,y)

let tmoveto_gtk tor = 
  let (x,y)= from_tortue tor.pos in
  point_courant := (x,y)

let tlineto_gtk tor line =
  let (x',y')= from_tortue tor.pos in
  point_courant := (x',y');
  List.append line [(float x'); (float y') ] 


let tdraw_string_gtk tor (ellipse : GnoCanvas.ellipse) =
  let (x,y) = from_tortue tor.pos in
(*            debug            *)
  if !debug then Format.eprintf "tdraw_string_gtk x=%d y=%d@." x y;
(*            /debug            *)
  moveto_gtk x y;
  ellipse#parent#move ~x:(float x) ~y:(float y);
  ellipse#parent#set  [`X (float x); `Y (float y)]


(* avance la tortue en traçant, d'une distance d, en un certain nombre d'etapes,
   et retourne la nouvelle position de la tortue *)
let tdraw_edge_gtk tor d etapes line =
  let d = d /. (float etapes) in
  let rec list_points t liste = function
    | 0 -> (t,liste)
    | n ->let t = advance t d in
	   list_points  t (tlineto_gtk t liste) (n-1)
  in
  let l = let (x,y) = from_tortue tor.pos in [(float x); (float y)] in 
  let t,lpoints = list_points tor l etapes in

(*            debug            *)
  if !debug 
  then
    (let ltext=
      let rec chaine = function
	|[]->""
	|e::l->(string_of_float e)^" "^chaine l
      in chaine lpoints in
      Format.eprintf "taille %d %s @." (List.length lpoints) ltext);
(*            /debug            *)
  let p = Array.of_list lpoints in
  line#set [`POINTS p];
  t


(* table of existing graphical nodes *)

module H = Hashtbl.Make(G.V)

let ellipses = H.create 97

let tdraw_string_gtk v tor canvas =
  let ellipse =
    try
      let item = H.find ellipses v in
      item#parent#show();
      item
    with Not_found ->
      let s = string_of_label v in
      let (w,h) = (40,20) in
      let noeud = GnoCanvas.group ~x:0.0 ~y:0.0 canvas in
      let ellipse = GnoCanvas.ellipse 
	~props:[ `X1  ( float_of_int (-w/2)); `Y1 (float_of_int (-h/2)); 
		 `X2  (float_of_int (w/2)) ; `Y2 ( float_of_int (h/2)) ;
		 `FILL_COLOR "grey" ; `OUTLINE_COLOR "black" ; 
		 `WIDTH_PIXELS 0 ] noeud  
      in
      let texte = GnoCanvas.text ~props:[`X 0.0; `Y 0.0 ; `TEXT s;  
				     `FILL_COLOR "blue"] noeud 
      in
      let w2 = texte#text_width in
      if w2 > float_of_int w
      then
	ellipse#set [ `X1  (-.( w2+.6.)/.2.); `X2 ((w2+.6.)/.2.)];
      H.add ellipses v ellipse;
      ellipse
  in
  tdraw_string_gtk tor ellipse;
  ellipse

(* tables of existing graphical edges *)

module H2 = 
  Hashtbl.Make
    (struct 
      type t = G.V.t * G.V.t
      let hash (v,w) = Hashtbl.hash (G.V.hash v, G.V.hash w)
      let equal (v1,w1) (v2,w2) = G.V.equal v1 v2 && G.V.equal w1 w2 
    end)

let grey_edges = H2.create 97
let black_edges = H2.create 97

let draw_grey_edge vw tv tw canvas =
  (*            debug            *)
  if  !debug
  then 
    ( 
      let (v,w)=  
	let (v,w) = vw in 
	(string_of_label v, string_of_label w) in
      eprintf "tortue %s \t tortue %s@." v w
    );
  (*            /debug            *)

  let p,l = 
    try
      let _,l as pl = H2.find grey_edges vw in
      l#show();
      pl
    with Not_found ->
      let p = GnomeCanvas.PathDef.new_path () in
      let l = GnoCanvas.bpath canvas
	~props:[ `BPATH p ; `OUTLINE_COLOR "SlateGrey" ; `WIDTH_PIXELS 1 ] in
      l#lower_to_bottom ();
      H2.add grey_edges vw (p,l);
      p,l
  in
  
  let (x,y) = let (x ,y ) = from_tortue tv.pos in ((float_of_int x),(float_of_int y)) in
  let (x',y') = let (x',y') = from_tortue tw.pos in ((float_of_int x'),(float_of_int y')) in
  let rapport = 1.95 in
  GnomeCanvas.PathDef.reset p;
  GnomeCanvas.PathDef.moveto p x y ;
  GnomeCanvas.PathDef.curveto p ((x+. x')/.rapport) ((y +. y')/.rapport) 
				 ((x  +.x')/.rapport) ((y +. y')/.rapport)
				 x' y';
  l#set [`BPATH p]

let tdraw_edge_gtk vw t distance etapes canvas =
 let line =
    try
      let l = H2.find black_edges vw in
      l#show ();
      l
    with Not_found ->
      let color = "black" in 
      let l = GnoCanvas.line canvas ~props:[ `FILL_COLOR color ;
					     `WIDTH_PIXELS 1; `SMOOTH true] 
      in
      H2.add black_edges vw l;
      l
 in
 tdraw_edge_gtk t distance etapes line

let color_change_intern_edge color node = 
  G.iter_edges
    (fun _ w ->
       try
	 let _,n = H2.find grey_edges (node,w) in
	 n#set [`OUTLINE_COLOR color]
       with Not_found ->
	 try
	   let _,n = H2.find grey_edges (w,node) in
	   n#set [`OUTLINE_COLOR color]
	 with Not_found ->
	   ()
    )
  !graph


let color_change_direct_edge color node = 
  G.iter_succ
    (fun w ->
       try
	 let n = H2.find black_edges (node,w) in
	 n#set [`FILL_COLOR color]
       with Not_found ->
	 try
	   let n = H2.find black_edges (w,node) in
	   n#set [`FILL_COLOR color]
	 with Not_found ->
	   ()
    )
    !graph node
  

let draw_graph canvas =
  (* nodes *)
  G.iter_vertex
    (fun v -> 
      let l = G.V.label v in
      if l.visible = Visible then 
	let _ = tdraw_string_gtk v l.turtle canvas in ()
    )
    !graph;
  (* edges *)

  (* intern edges *)
  G.iter_edges
    (fun v w ->
      let labv = G.V.label v in
      let labw = G.V.label w in
      let lv = labv.depth in
      let tv = labv.turtle in
      let lw = labw.depth in
      let tw = labw.turtle in
      if labv.visible = Visible && labw.visible = Visible && 
	 abs (lw - lv) <> 1 && (lv <> 0 || lw <> 0) 
      then begin
	(*            debug            *)
	if !debug 
	then
	  (Format.eprintf "tortue : %s\t\t\t tortue : %s@." 
	      (string_of_label v) (string_of_label w);
	   let (x ,y ) = from_tortue tv.pos 
	   and (x',y') = from_tortue tw.pos in
	   Format.eprintf "pos  x:%d y:%d \t pos x:%d y:%d@." x y x' y';
	  );
	(*            /debug           *)
	ignore (draw_grey_edge (v,w) tv tw canvas)
      end 
    ) 
    !graph
