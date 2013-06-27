
module type G = sig
  type t 
  module V : Sig.COMPARABLE 
  module E : sig 
    type t 
    type label 
    val label : t -> label 
    val dst : t -> V.t 
    val src : t -> V.t
    val compare : t -> t -> int
  end 
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_edges_e : (E.t -> unit) -> t ->  unit
end

(** Functor providing an implementation of Kruskal's minimum-spanning-tree 
    algorithm. 
    Parameter [W] ensures that label on edges are comparable. *)
module Make(G: G)(W: Sig.ORDERED_TYPE with type t = G.E.label) : sig
  val spanningtree : G.t -> G.E.t list
end  

