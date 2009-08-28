(**************************************************************************)
(*                                                                        *)
(*  This file is part of OcamlGraph.                                      *)
(*                                                                        *)
(*  Copyright (C) 2009                                                    *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1, with a linking exception.                    *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the file ../LICENSE for more details.                             *)
(*                                                                        *)
(*  Authors:                                                              *)
(*    - Jean-Denis Koeck (jdkoeck@gmail.com)                              *)
(*    - Julien Signoles  (Julien.Signoles@cea.fr)                         *)
(*                                                                        *)
(**************************************************************************)

open Graph
open DGraphModel

(** Signature of an OCamlGraph module with labels as DOT layouts *)
module type G = Sig.G with type V.label = DGraphModel.node_layout
		      and type E.label = DGraphModel.edge_layout

(** Signature of a module implementing a concrete model *)
module type S = sig
  module G: G
  type cluster
  type clusters_hash = (cluster, Graph.Dot_ast.attr list) Hashtbl.t
  class model : G.t -> clusters_hash -> bounding_box ->
  object
    method bounding_box : bounding_box
    method find_edge : G.vertex -> G.vertex -> G.edge
    method get_edge_layout : G.edge -> G.E.label
    method get_vertex_layout : G.vertex -> G.V.label
    method get_cluster_layout : cluster -> cluster_layout
    method iter_edges : (G.vertex -> G.vertex -> unit) -> unit
    method iter_edges_e : (G.edge -> unit) -> unit
    method iter_pred : (G.vertex -> unit) -> G.vertex -> unit
    method iter_pred_e : (G.edge -> unit) -> G.vertex -> unit
    method iter_succ : (G.vertex -> unit) -> G.vertex -> unit
    method iter_succ_e : (G.edge -> unit) -> G.vertex -> unit
    method iter_vertex : (G.vertex -> unit) -> unit
    method iter_clusters : (cluster -> unit) -> unit
    method mem_edge : G.vertex -> G.vertex -> bool
    method mem_edge_e : G.edge -> bool
    method mem_vertex : G.vertex -> bool
  end
  val model : G.t -> clusters_hash -> bounding_box -> model
end

(** Instantiates a concrete model from an OCamlGraph module *)
module Make(G : G) = struct 
  module G = G
  type cluster = string
  type clusters_hash = (cluster, Graph.Dot_ast.attr list) Hashtbl.t

  class model graph clusters_hash bounding_box =
    let g = graph in
  object
    inherit [G.V.t, G.E.t, cluster] DGraphModel.model

    (* Iterators *)
    method iter_edges f = G.iter_edges f g
    method iter_edges_e f = G.iter_edges_e f g
    method iter_pred f v = G.iter_pred f g v
    method iter_pred_e f v = G.iter_pred_e f g v
    method iter_succ f = G.iter_succ f g
    method iter_succ_e f = G.iter_succ_e f g 
    method iter_vertex f = G.iter_vertex f g
    method iter_clusters f = Hashtbl.iter (fun k _ -> f k) clusters_hash

    (* Membership functions *)
    method find_edge = G.find_edge g
    method mem_edge = G.mem_edge g
    method mem_edge_e = G.mem_edge_e g
    method mem_vertex = G.mem_vertex g

    (* Layout *)
    method bounding_box = bounding_box
    method get_vertex_layout = G.V.label
    method get_edge_layout = G.E.label
    method get_cluster_layout c =
      let attrs = Hashtbl.find clusters_hash c in
      XDot.read_cluster_layout attrs
  end

  let model = new model
end

(* DOT PARSING *)

module Vertex = struct
  type t = DGraphModel.node_layout
end

module Edge = struct
  type t = DGraphModel.edge_layout
  let default = DGraphModel.mk_edge_layout
    ~draw:[] ~ldraw:[] ~hdraw:[] ~tdraw:[] ~hldraw:[] ~tldraw:[]
  let compare = compare
end

module G = Imperative.Digraph.AbstractLabeled(Vertex)(Edge)
module B = Builder.I(G)

module DotParser =
  Dot.Parse
    (B)
    (struct 
       (* Read the attributes of a node *)
       let node _id = XDot.read_node_layout

       (* Read edge attributes *)
       let edge = XDot.read_edge_layout

     end)

(* GRAPH MODULE *)

module Model = Make(G)

exception DotError of string

let read_dot ?(cmd="dot") ~dot_file =
  let basename = try Filename.chop_extension dot_file 
                 with Invalid_argument _ -> dot_file in
  let xdot_file = basename ^ ".xdot" in
  let dot_cmd = Printf.sprintf "%s -Txdot %s > %s" cmd dot_file xdot_file in

  (* Run graphviz *)
  match Sys.command dot_cmd with
    | 0 -> begin
	let graph, bb, clusters_hash =
	  DotParser.parse_bounding_box_clusters xdot_file in
	Model.model graph clusters_hash (XDot.read_bounding_box bb)
      end
    | _ -> raise (DotError "Error during dot execution")
