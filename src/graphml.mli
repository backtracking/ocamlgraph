(******************************************************************************)
(*                                                                            *)
(*  Copyright (C) 2012 Pietro Abate <pietro.abate@pps.jussieu.fr>             *)
(*                                                                            *)
(*  This library is free software: you can redistribute it and/or modify      *)
(*  it under the terms of the GNU Lesser General Public License as            *)
(*  published by the Free Software Foundation, either version 3 of the        *)
(*  License, or (at your option) any later version.  A special linking        *)
(*  exception to the GNU Lesser General Public License applies to this        *)
(*  library, see the COPYING file for more information.                       *)
(*                                                                            *)
(******************************************************************************)

(** Generic GraphMl Printer *)

(** Graph information required by Graphml *)
module type G = sig

  include Sig.G

  val vertex_properties : (string * string * string option) list
  (** List of the type of the vertex proprierties.
      The format is (id,type,default). *)

  val edge_properties : (string * string * string option) list
  (** List of the type of the edge proprierties. *)

  val map_vertex : vertex -> (string * string) list
  (** Associates to each vertex a key/value list where
      the key is the id of a vertex attribute and the value is the value
      associated to this vertex *)

  val map_edge : edge -> (string * string) list
  (** Associates to each edge a key/value list *)

  val vertex_uid : vertex -> int
  (** @return a unique integer identifier for the vertex *)

  val edge_uid : edge -> int
  (** @return a unique integer identifier for the edge *)

end

(** Graphml printer signature *)
module type S = sig
  type t
  val fprintf : out_channel -> t -> unit
(** [fprintf oc graph] print the graphml representation of the given graph 
    on the out channel oc *)
end

(** Graphml Printer given a graph and required info *)
module Printer(G: G): S with type t = G.t

(*
Local Variables:
compile-command: "make -C .."
End:
*)
