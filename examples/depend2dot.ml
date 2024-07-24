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

open Graph

let usage () =
  Format.eprintf "usage: depend2dot@.";
  Format.eprintf "reads a dependency graph on the standard input, in format@.";
  Format.eprintf "  a: b c d@.";
  Format.eprintf "  b: c e@.";
  Format.eprintf "  etc.@.";
  Format.eprintf "and prints a reduced graph in DOT format on the standard output.@.";
  exit 1

module G = Imperative.Digraph.Abstract(String)
module O = Oper.Make(Builder.I(G))
module H = Hashtbl

let graph = G.create ()

let () =
  let nodes = H.create 16 in
  let node s = try H.find nodes s
               with Not_found -> let v = G.V.create s in H.add nodes s v; v in
  let node s = node (String.trim s) in
  let add v w = if w <> "" then G.add_edge graph (node v) (node w) in
  let add v w = add v w in
  let parse_line s = match String.split_on_char ':' s with
    | [v; deps] -> List.iter (add v) (String.split_on_char ' ' deps)
    | [_] -> ()
    | _ -> usage () in
  let rec read () = match read_line () with
    | s -> parse_line s; read ()
    | exception End_of_file -> () in
  read ()

let graph = O.replace_by_transitive_reduction graph

module Display = struct
  include G
  let vertex_name = V.label
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end
module Dot = Graphviz.Dot(Display)

let () = Dot.output_graph stdout graph
