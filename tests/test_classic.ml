(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2007                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Graph.Pack.Graph (* undirected graphs *)

let g = Classic.petersen ()
let () = assert (nb_vertex g = 10)
let () = assert (nb_edges  g = 15)
let () = dot_output g "petersen.dot"

let g = Classic.kneser ~n:7 ~k:3
let () = dot_output g "k_7_3.dot"

let g = Classic.kneser ~n:0 ~k:0
let () = assert (nb_vertex g = 1)
let () = assert (nb_edges  g = 0)

let g = Classic.kneser ~n:1 ~k:0
let () = assert (nb_vertex g = 1)
let () = assert (nb_edges  g = 0)

let g = Classic.kneser ~n:1 ~k:1
let () = assert (nb_vertex g = 1)
let () = assert (nb_edges  g = 0)

let g = Classic.kneser ~n:2 ~k:1
let () = assert (nb_vertex g = 2)
let () = assert (nb_edges  g = 1)
