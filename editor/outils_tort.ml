
open Graphics
open Outils_math

let debug_outil_tort =  ref false

let (w,h)= (600.,600.)

(*** Tortue Hyperbolique ***)


type coord = float * float 

type turtle =
    {
      pos : coord ;  (* with |pos| < 1 *)
      dir : coord    (* with |dir| = 1 *)
    } 

let make_turtle pos angle =
  { 
    pos = pos ;
    dir = expi angle 
  }

let make_turtle_dir pos dir =
  { 
    pos = pos ;
    dir = dir 
  }


let advance turt step =
   { pos = gamma turt.pos turt.dir step ;
     dir = delta turt.pos turt.dir step }
   

let turn turtle u =
  { turtle with dir = turtle.dir *& u }

let turn_left turtle angle =
  turn turtle (expi angle)       (*** a comprendre pourquoi je dois inverser + et - de l'angle ***)

let turn_right turtle angle =
  turn turtle (expi (-.angle))           (*** a comprendre pourquoi je dois inverser + et - de l'angle ***) 

let to_tortue(x,y)=
  ((float x*.(2./.w) -. 1.),(1. -. float y *.(2./.h)))
(*  ((float x*.(2./.w) ),(float y *.(2./.h) ))*)

let from_tortue (x,y) =
  let xzoom = (w/.2.)
  and yzoom = (h/.2.) in
  (truncate (x*.xzoom +. xzoom), truncate(yzoom -. y*.yzoom))

let origine =ref (to_tortue (truncate(w/.2.), truncate(h/.2.)))




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
  if !debug_outil_tort then Format.eprintf "tdraw_string_gtk x=%d y=%d@." x y;
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
  let l = let (x,y) =from_tortue tor.pos in [(float x); (float y)] in 
  let t,lpoints = list_points tor l etapes in

(*            debug            *)
  if (!debug_outil_tort) 
  then
    (let ltext=
      let rec chaine = function
	|[]->""
	|e::l->(string_of_float e)^" "^chaine l
      in chaine lpoints in
      Format.eprintf "taille %d %s @." (List.length lpoints) ltext);

  let p = Array.of_list lpoints in
  line#set [`POINTS p];
  t
