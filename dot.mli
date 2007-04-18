(*
 * Graph: generic graph library
 * Copyright (C) 2004
 * Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 *)

(* $Id:$ *)

(** Parser for DOT file format *)

open Dot_ast

module Parse 
  (B : Builder.S)
  (L : sig 
     val node : node_id -> attr list -> B.G.V.label
       (** how to build the node label out of the set of attributes *)
     val edge : attr list -> B.G.E.label 
       (** how to build the edge label out of the set of attributes *)
   end) :
sig
  
  val parse : string -> B.G.t

end
