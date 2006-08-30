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



(* Unionfind structure over hash-ordered types.

   This module implements a unionfind data structure, given a total ordering
   function and a hash function over the elements. 

*)


module type HashedOrderedType = sig
  (* The type of the elements*)
  type t
  val equal : t -> t -> bool
  val hash : t -> int 
  val compare : t -> t -> int 
end

(* Input signature of the functor Unionfind.Make *)

module type S = sig
  type elt
  type t
    
  val init : elt list -> t
  val find : elt -> t -> elt
  val union : elt -> elt -> t -> unit
end

module Make (X : HashedOrderedType) : S with type elt = X.t


