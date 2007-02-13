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



module Make(T : TREE) : GTREE with type t = T.t and type label = T.label

