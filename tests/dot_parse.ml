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

(* $Id:$ *)

open Graph
module G = Imperative.Digraph.Abstract(String)
module B = Builder.I(G)
module P = struct 
  let node (id,_) _ = match id with
| Dot_ast.Ident s
| Dot_ast.Number s
| Dot_ast.String s
| Dot_ast.Html s -> s
  let edge _ = ()
end

module DotInput = Dot.Parse (B) (P)
module DotInput_V1 = Dot_V1.Parse (B) (P)

let g, _, _ = DotInput.parse_all Sys.argv.(1)
let g_v1 = DotInput_V1.parse Sys.argv.(1)

let include_vertex g1 g2 =
  G.fold_vertex (fun v1 is_mem -> is_mem && G.mem_vertex g2 v1) g1 true 

let include_edge g1 g2 =
  G.fold_edges_e (fun e1 is_mem -> is_mem && G.mem_edge_e g2 e1) g1 true 

let quasi_equal_graph g1 g2 = 
  (G.nb_edges g1 = G.nb_edges g2)
  && (G.nb_vertex g1 = G.nb_vertex g2)
  (* && include_vertex g1 g2
   && include_edge g1 g2 *)

let () = assert (quasi_equal_graph g g_v1)
