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

open Format
open Graph

module C = Components.Undirected(Pack.Graph)

open Pack.Graph

(*         0 -- 2 -- 7   1   3 -- 4   5
                             \  /
                               6

component:      0        1     2      3
*)

let () =
  let g = create () in
  let v = Array.init 8 V.create in
  Array.iter (add_vertex g) v;
  let add i j = add_edge g v.(i) v.(j) in
  add 0 2; add 7 2; add 3 4; add 4 6; add 3 6;
  let n, f = C.components g in
  printf "%d components@." n;
  iter_vertex (fun v -> printf "%d -> %d@." (V.label v) (f v)) g

