module type TREE = sig
  type t
  type label
  val children : t -> t list
  val label : t -> label
end ;;

module type HTREE = sig
  type t
  type label
  val children : t -> t list
  val label : t -> label

  type coord = float * float

  type driver = {
      rlimit : float ;
      moveto : coord -> unit ;
      lineto : coord -> unit ;
      curveto : coord -> coord -> coord -> unit ;
      draw_label : label -> coord -> float -> unit ;
      init_edge_pass : unit -> unit ;
      init_label_pass : unit -> unit ;
      finalize : unit -> unit ;
    }

  val shrink_factor : coord -> float
  val drag_origin : coord -> coord -> coord -> coord

  val draw_linear_tree : driver -> t -> coord -> float -> unit
  val draw_curved_tree : driver -> t -> coord -> float -> unit
end ;;

module Make(T : TREE) :
    HTREE with type t = T.t and type label = T.label ;;
