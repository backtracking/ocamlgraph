(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2008                                               *)
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
   end) =
struct

  module Attr = struct
    module M = Map.Make(struct type t = id let compare = compare end)
    type t = id option M.t
    let empty = M.empty
    let add = List.fold_left (fun a (x,v) -> M.add x v a)
    let addl = List.fold_left add
    let list a = M.fold (fun x v l -> (x,v) :: l) a []
  end

  let create_graph dot =
    (* pass 1: collect node attributes, in table node_attr *)
    let def_node_attr = ref Attr.empty in
    let node_attr = Hashtbl.create 97 in
    let add_node_attr id al =
      let l = try Hashtbl.find node_attr id with Not_found -> !def_node_attr in
      Hashtbl.replace node_attr id (Attr.addl l al)
    in
    let rec collect_node_attr stmts =
      List.iter (
        function
          | Node_stmt (id, al) -> add_node_attr id al
          | Attr_node al -> def_node_attr := Attr.addl !def_node_attr al
          | Edge_stmt (NodeId id, nl, _) -> 
              add_node_attr id [];
              List.iter (function NodeId id -> add_node_attr id [] | _ -> ()) nl
          | Subgraph (SubgraphDef (_, stmts)) -> collect_node_attr stmts
          | _ -> ()
      ) stmts
    in
    collect_node_attr dot.stmts;
    (* pass 2: build the graph *)
    let def_edge_attr = ref Attr.empty in
    let nodes = Hashtbl.create 97 in
    let node g id l =
      try
	g, Hashtbl.find nodes id
      with Not_found ->
	let l = try Hashtbl.find node_attr id with Not_found -> Attr.empty in
	let n = B.G.V.create (L.node id [Attr.list l]) in
	Hashtbl.add nodes id n;
	B.add_vertex g n, n
    in
    let rec add_stmts g stmts =
      List.fold_left
        (fun g s -> match s with
           | Node_stmt (id, al) ->
               let g,_ = node g id al in g
           | Edge_stmt (NodeId id, nl, al) ->
               let al = Attr.addl !def_edge_attr al in
               let el = L.edge [Attr.list al] in
               let g,vn = node g id [] in
                 List.fold_left
                   (fun g m -> match m with
                      | NodeId idm -> 
                          let g,vm = node g idm [] in
                          let e = B.G.E.create vn el vm in
                            B.add_edge_e g e
                      | NodeSub _ -> 
                          g)
                   g nl
           | Attr_edge al -> 
               def_edge_attr := Attr.addl !def_edge_attr al; g
           | Subgraph (SubgraphDef (_, stmts)) ->
               add_stmts g stmts
           | _ -> g
        )
        g stmts
    in add_stmts (B.empty ()) dot.stmts

  let parse f =
    let c = open_in f in
    let lb = Lexing.from_channel c in
    let dot = 
      try
	Dot_parser.file Dot_lexer.token lb 
      with Parsing.Parse_error ->
	let n = Lexing.lexeme_start lb in
	failwith (Printf.sprintf "Dot.parse: parse error character %d" n)
    in
    close_in c;
    create_graph dot

end
