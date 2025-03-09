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

(** Double-ended queue *)

type 'a t

val create: unit -> 'a t
val length: 'a t -> int

val clear: 'a t -> unit

val push_front: 'a t -> 'a -> unit
val peek_front: 'a t -> 'a
val pop_front: 'a t -> 'a

val push_back: 'a t -> 'a -> unit
val peek_back: 'a t -> 'a
val pop_back: 'a t -> 'a
