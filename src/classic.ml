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

(* $Id: classic.ml,v 1.9 2004-02-02 08:11:14 filliatr Exp $ *)

module type S = sig
  type graph
  type vertex
  val divisors : int -> graph
  val de_bruijn : int -> graph
  val vertex_only : int -> graph
  val full : ?self:bool -> int -> graph
  val cycle : int -> graph * vertex array
  val grid : n:int -> m:int -> graph * vertex array array
end

module Generic(B : Builder.INT) = struct

  type graph = B.G.t

  type vertex = B.G.V.t

  let divisors n =
    if n < 2 then invalid_arg "divisors";
    let v = Array.init (n + 1) (fun i -> B.G.V.create i) in
    let rec loop g i =
      let sqrt_i = truncate (sqrt (float i)) in
      let rec loop_i g d =
        if d > sqrt_i then
          g
        else if i mod d == 0 then
          loop_i (B.add_edge (B.add_edge g v.(i / d) v.(i)) v.(d) v.(i)) (d+1)
        else
          loop_i g (succ d)
      in
      if i > n then g else loop (loop_i (B.add_vertex g v.(i)) 2) (i+1)
    in
    loop (B.empty ()) 2

  let fold_for i0 i1 f =
    let rec loop i v = if i > i1 then v else loop (i + 1) (f v i) in
    loop i0

  let de_bruijn n =
    if n < 1 || n > Sys.word_size - 1 then invalid_arg "de_bruijn";
    let v = Array.init (1 lsl n) (fun i -> B.G.V.create i) in
    let all_1 = 1 lsl n - 1 in (* 11...1 *)
    let g = fold_for 0 all_1 (fun g i -> B.add_vertex g v.(i)) (B.empty ()) in
    let rec loop g i =
      if i > all_1 then
        g
      else
        let si = (i lsl 1) land all_1 in
        let g = B.add_edge g v.(i) v.(si) in
        let g = B.add_edge g v.(i) v.(si lor 1) in
        loop g (i + 1)
    in
    loop g 0

  let vertex_only n =
    fold_for 1 n (fun g i -> B.add_vertex g (B.G.V.create i)) (B.empty ())

  let full ?(self=true) n =
    let v = Array.init (n + 1) (fun i -> B.G.V.create i) in
    fold_for 1 n
      (fun g i ->
         fold_for 1 n
           (fun g j -> if self || i <> j then B.add_edge g v.(i) v.(j) else g)
           g)
      (fold_for 1 n (fun g i -> B.add_vertex g v.(i)) (B.empty ()))

  let cycle n =
    if n < 0 then invalid_arg "cycle";
    let v = Array.init n (fun i -> B.G.V.create i) in
    let g = Array.fold_left B.add_vertex (B.empty ()) v in
    let rec loop g i =
      if i = n then g
      else let g = B.add_edge g v.(i) v.((i+1) mod n) in loop g (i+1) in
    loop g 0, v

  let grid ~n ~m =
    if n < 0 || m < 0 then invalid_arg "grid";
    let create i j = B.G.V.create (m * i + j) in
    let v = Array.init n (fun i -> Array.init m (fun j -> create i j)) in
    let g = Array.fold_left (Array.fold_left B.add_vertex) (B.empty ()) v in
    let rec loop g i j =
      if i = n then g
      else if j = m then loop g (i+1) 0
      else let g = if j < m-1 then B.add_edge g v.(i).(j) v.(i).(j+1) else g in
           let g = if i < n-1 then B.add_edge g v.(i).(j) v.(i+1).(j) else g in
           loop g i (j+1) in
    loop g 0 0, v

end

module P (G : Sig.P with type V.label = int) = Generic(Builder.P(G))

module I (G : Sig.I with type V.label = int) = Generic(Builder.I(G))
