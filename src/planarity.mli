
(** minimal signature for {!Make}.
    Sub-signature of {!Sig.G}. *)
module type G = sig
  type t
end

module Make(G: G) : sig

  val is_planar : G.t -> bool
end
