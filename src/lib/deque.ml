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

(** Double-ended queue implemented with doubly-linked lists *)

type 'a cell =
  | Null
  | Cell of { value: 'a; mutable prev: 'a cell; mutable next: 'a cell; }

type 'a t = {
  mutable front: 'a cell;
  mutable back : 'a cell;
  mutable size : int;
}
  (* invariant: size=0 && front=back == Null
             || size>0 && front,back != Null

            ----next--->
      front               back
            <---prev----
  *)

let create () =
  { front = Null; back = Null; size = 0 }

let length dq =
  dq.size

let clear dq =
  dq.front <- Null;
  dq.back <- Null;
  dq.size <- 0

let add_first dq x =
  let c = Cell { value = x; prev = Null; next = Null } in
  dq.front <- c;
  dq.back <- c;
  dq.size <- 1

let push_front dq x =
  match dq.front with
  | Null ->
      add_first dq x
  | Cell f as cf ->
      let c = Cell { value = x; prev = Null; next = cf } in
      f.prev <- c;
      dq.front <- c;
      dq.size <- dq.size + 1

let peek_front dq =
  match dq.front with
  | Null                -> invalid_arg "peek_front"
  | Cell { value=v; _ } -> v

let pop_front dq =
  match dq.front with
  | Null ->
      invalid_arg "pop_front"
  | Cell { value=v; next=Null; _} ->
      clear dq;
      v
  | Cell { value=v; next=Cell c as n; _} ->
      dq.front <- n;
      c.prev <- Null;
      dq.size <- dq.size - 1;
      v

let push_back dq x =
  match dq.back with
  | Null ->
      add_first dq x
  | Cell b as cb ->
      let c = Cell { value = x; prev = cb; next = Null } in
      b.next <- c;
      dq.back <- c;
      dq.size <- dq.size + 1

let peek_back dq =
  match dq.back with
  | Null                -> invalid_arg "peek_back"
  | Cell { value=v; _ } -> v

let pop_back dq =
  match dq.back with
  | Null ->
      invalid_arg "pop_back"
  | Cell { value=v; prev=Null; _} ->
      clear dq;
      v
  | Cell { value=v; prev=Cell c as p; _} ->
      dq.back <- p;
      c.next <- Null;
      dq.size <- dq.size - 1;
      v
