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
module DotInput = 
  Dot.Parse
    (B)
    (struct 
      let node (id,_) _ = match id with
	| Dot_ast.Ident s
	| Dot_ast.Number s
	| Dot_ast.String s
	| Dot_ast.Html s -> s
      let edge _ = ()
    end)
module Display = struct
  include G
  let vertex_name v = "\"" ^ String.escaped (V.label v) ^ "\""
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = [ `HtmlLabel "f&#36;oo"; `Tooltip "description"  ]
  let get_subgraph _ = None
end
module DotOutput = Graphviz.Dot(Display)

let g = DotInput.parse Sys.argv.(1)

let () = 
  Printf.printf "========= BEGIN output graph =========\n";
  DotOutput.output_graph stdout g;
  Printf.printf "========= END output graph ========="