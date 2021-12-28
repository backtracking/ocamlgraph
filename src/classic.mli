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

(** Some classic graphs *)

module type S = sig

  type graph

  type vertex

  val divisors : int -> graph
  (** [divisors n] builds the graph of divisors.
      Vertices are integers from [2] to [n]. [i] is connected to [j] if
      and only if [i] divides [j].
      @raise Invalid_argument is [n < 2]. *)

  val de_bruijn : int -> graph
  (** [de_bruijn n] builds the de Bruijn graph of order [n].
      Vertices are bit sequences of length [n] (encoded as their
      interpretation as binary integers). The sequence [xw] is connected
      to the sequence [wy] for any bits [x] and [y] and any bit sequence
      [w] of length [n-1].
      @raise Invalid_argument is [n < 1] or [n > Sys.word_size-1]. *)

  val vertex_only : int -> graph
  (** [vertex_only n] builds a graph with [n] vertices and no edge. *)

  val full : ?self:bool -> int -> graph
  (** [full n] builds a graph with [n] vertices and all possible edges.
      The optional argument [self] indicates if loop edges should be added
      (default value is [true]). *)

  val cycle : int -> graph * vertex array
  (** [cycle n] builds a graph that is a cycle with [n] vertices.
      Vertices are labelled with [0,1,...,n-1] and there is an edge from
      vertex [i] to vertex [(i+1) mod n].
      Vertices are also returned in an array for convenience. *)

  val grid : n:int -> m:int -> graph * vertex array array
  (** [grid n m] builds a grid graph with [n*m] vertices, with edges
      from vertex [(i,j)] to vertices [(i+1,j)] and [(i,j+1)] (and no
      wrapping around). Vertex [(i,j)] is labelled with [i*m+j].
      Vertices are also returned in a [n*m] matrix for convenience. *)

end

module P (G : Sig.P with type V.label = int) :
   S with type graph = G.t and type vertex = G.V.t
(** Classic Persistent Graphs *)

module I (G : Sig.I with type V.label = int) :
   S with type graph = G.t and type vertex = G.V.t
(** Classic Imperative Graphs *)
