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

module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end

module G = Persistent.Digraph.ConcreteLabeled(Int)(Int)

let g = G.empty
let g = G.add_vertex g 1
let g = G.add_edge_e g (G.E.create 1 10 2)
let g = G.add_edge_e g (G.E.create 2 50 3)
let g = G.add_edge_e g (G.E.create 1 30 4)
let g = G.add_edge_e g (G.E.create 1 100 5)
let g = G.add_edge_e g (G.E.create 3 10 5)
let g = G.add_edge_e g (G.E.create 4 20 3)
let g = G.add_edge_e g (G.E.create 4 60 5)

let g = G.remove_vertex g 4

let gc = G.add_edge_e g (G.E.create 5 10 1)
let gc = G.add_vertex gc 6

module W = struct
  type edge = G.E.t
  type t = int
  let weight e = G.E.label e
  let zero = 0
  let add = (+)
  let sub = (-)
  let compare = compare
end

module Dij = Path.Dijkstra(G)(W)

let p,w = Dij.shortest_path gc 1 5

open G.E

let () = List.iter (fun e -> printf "[%d -> %d]" (src e) (dst e)) p; printf "@."

module Comp = Components.Make(G)
let g = G.add_edge g 3 2
let n, f = Comp.scc g
let () = G.iter_edges (fun u v -> printf "%d -> %d@." u v) g
let () = printf "%d components@." n
let () = G.iter_vertex (fun v -> printf "  %d -> %d@." v (f v)) g

(* dominators *)

module G = Imperative.Digraph.ConcreteLabeled(Int)(Int)
module D = Dominator.Make(G)
module R = Rand.Make(Builder.I(G))
module C = Components.Make(G)
let g = R.graph ~v:200_000 ~e:200_000 ()
let n, f = C.scc g
let () = printf "%d@." n
let () = for i = 0 to 9 do printf "%d " (f i) done; printf "@."
(*
199958
92100 26941 198885 157971 91597 87047 141830 173559 68202 77266
*)

let g = R.graph ~v:200_000 ~e:1_000_000 ()
let n, f = C.scc g

