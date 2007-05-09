
(*** Complex numbers *)

let ( ~-& ) (x, y) = (-.x, -.y) 
let ( ~& ) (x, y) = (x, -.y) 

let ( +& ) (x1, y1) (x2, y2) =
  (x1 +. x2, y1 +. y2) 

let ( -& ) (x1, y1) (x2, y2) =
  (x1 +. x2, y1 +. y2) 

let ( *& ) (x1, y1) (x2, y2) =
  (x1*.x2 -. y1*.y2, x1*.y2 +. y1*.x2) 

let ( /& ) (x1, y1) (x2, y2) =
  let n2 = x2*.x2 +. y2*.y2 in
  ((x1*.x2 +. y1*.y2)/.n2, (-.x1*.y2 +. y1*.x2)/.n2) 

let ( *.& ) f (x, y) =
  (f*.x, f*.y) 

let norm_sqr (x, y) =
  x*.x +. y*.y 

let norm (x, y) =
  sqrt(x*.x +. y*.y) 

let normalize (x, y) =
  let n = sqrt(x*.x +. y*.y) in
  (x/.n, y/.n) 

let expi t =
  (cos t, sin t) 

(*** Hyperbolic geometry ***)

let th t =
  let ept = exp t
  and emt = exp (-.t) in
  (ept -. emt)/.(ept +. emt)

let ath x =
  0.5*.log((1.0 +. x)/.(1.0 -. x)) 

let pi = 3.14159265358979323846 
let pi_over_2 = pi/.2.0 
let pi_over_4 = pi/.4.0 

let one = (1.0, 0.0) 

let translate a z =
  (a +& z)/&(one +& (~&a) *& z) 

let gamma a u t =
  let utht = th t *.& u in
  (a +& utht) /& (one +& (~&a) *& utht) 

let delta a u t =
  let atht = th t *.& a
  and utht = th t *.& u in
  normalize ((u +& atht) /& (one +& (~&a) *& utht)) 

(* solving a Cramer system *)
let cramer a1 a2 b1 b2 c1 c2 =
  let cdet = a1*.b2 -. a2*.b1
  and xdet = c1*.b2 -. c2*.b1
  and ydet = a1*.c2 -. a2*.c1 in
  (xdet/.cdet, ydet/.cdet) ;;

let drag_origin (x0, y0) (x1, y1) (x2, y2) =
  let (x1, y1) = translate (-.x0, -.y0) (x1, y1) in
  let x3 = x1*.x2 -. y1*.y2 in
  let y3 = x1*.y2 +. y1*.x2 in
  cramer (1.0 -. x3) (-.y3) (-.y3) (1.0 +. x3) (x2 -. x1) (y2 -. y1)


(*** Hyperbolic turtle ***)

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

let advance_many turt d steps =
  let d = d /. (float steps) in
  let rec adv t = function 
    | 0 -> t
    | n -> adv (advance t d) (n-1)
  in
  adv turt steps

let turn turtle u =
  { turtle with dir = turtle.dir *& u }

let turn_left turtle angle =
  turn turtle (expi angle)       (*** a comprendre pourquoi je dois inverser + et - de l'angle ***)

let turn_right turtle angle =
  turn turtle (expi (-.angle))           (*** a comprendre pourquoi je dois inverser + et - de l'angle ***) 

let dummy_turtle = { pos = (0., 0.); dir = (0., 0.) }


(* [step_from n] computes the best `distance' for solving the
   dictator's problem in the complex hyperbolic plane for [n]
   dictators.  In a half-plane, we have to use the distance
   given by [step_from (2*n)] or, better, the distance given
   by [step_from (2*max(3 n))]. *)
let step_from n =
  ath (tan (pi_over_4 -. pi/.float(2*n)))


(* [hspace_dist_sqr turtle] computes the square of the distance
   between the origin and the half-space in front of [turtle]. *)
let hspace_dist_sqr turtle =
  let (ax, ay) = turtle.pos
  and (dx, dy) = turtle.dir in
 (* if ax*.dx +. ay*.dy < 0.0 then 0.0 else*)
  begin
    let ux = dy and uy = -.dx in
    let alpha = ax*.ax +. ay*.ay
    and beta = 2.0*.(ax*.ux +. ay*.uy) in
    if beta = 0.0 then
      alpha
    else
      begin
	let gamma = (1.0 +. alpha)/.beta in
	let delta = gamma*.gamma -. 1.0 in
	let sol =
          if beta > 0.0
          then -.gamma +. sqrt(delta)
          else -.gamma -. sqrt(delta) in
	let (zx, zy) = translate (ax, ay) (ux*.sol, uy*.sol) in
	zx*.zx +. zy*.zy
      end
  end


