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

  val show_tree : GnoCanvas.canvas -> t -> int -> int -> unit
end

module Make(T : TREE) = struct
  type t = T.t
  type label = T.label
  let children = T.children
  let label = T.label
  let string_of_label = T.string_of_label

  module HT = Htree.Make(T)

  let show_tree canvas t width height =
    let rlimit = 0.98
    and xzoom = float(width)/.2.0
    and yzoom = float(height)/.2.0 in

    let current_x = ref 0.0 in
    let current_y = ref 0.0 in

    let gtk_moveto (zx, zy) =
      current_x := zx; current_y := zy
    in
    let gtk_lineto (zx, zy) =
      assert false (*TODO*)
    in
    assert false (*TODO*)

end

