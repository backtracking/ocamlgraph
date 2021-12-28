(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Eulerian path

    This module implements Hierholzer's algorithm, with O(E) complexity
    where E is the number of edges.

    Limitations:
    - multigraphs are not supported
 *)

module type G = sig
  type t
  val is_directed : bool
  module V : Sig.COMPARABLE
  module E : Sig.EDGE with type vertex = V.t
  val iter_edges_e : (E.t -> unit) -> t -> unit
end

module Make(G: G) : sig

  val path: G.t -> G.E.t list * bool
  (** [path g] returns an Eulerian path of [g]. The Boolean indicates
      whether the path is a cycle. Raises [Invalid_argument] if there
      is no Eulerian path. *)

  val cycle: G.t -> G.E.t list
  (** [cycle g] returns an Eulerian cycle of [g].
      Raises [Invalid_argument] if there is no Eulerian cycle. *)

end
