(*
module T : sig 
  type t = DirTree.t 
  val root : t
end
*)

module T : sig 
  type t
  val root : t
end


val show_tree : #GnoCanvas.group -> T.t -> int -> int -> unit
