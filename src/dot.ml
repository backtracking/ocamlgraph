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

(** Parser for DOT file format *)

open Dot_ast

let parse_dot_ast_from_chan c =
  let lb = Lexing.from_channel c in
  let dot =
    try
      Dot_parser.file Dot_lexer.token lb
    with Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Dot.parse: parse error character %d" n)
  in
  close_in c;
  dot

let parse_dot_ast f =
  let c = open_in f in
  parse_dot_ast_from_chan c

type clusters_hash = (string, attr list) Hashtbl.t

type graph = {
  sg_nodes : string list;
  sg_attr : attr list;
  sg_parent : string option;
}

type graph_hash = (string option, graph) Hashtbl.t

let get_string = function
  | String s -> s
  | Ident s -> s
  | Number s -> s
  | Html s -> s

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
    module M = 
      Map.Make
        (struct
          type t = id
          let compare : t -> t -> int = Stdlib.compare
        end)
    let empty = M.empty
    let add = List.fold_left (fun a (x,v) -> M.add x v a)
    let addl = List.fold_left add
    let list a = M.fold (fun x v l -> (x,v) :: l) a []
  end

  module Node_set = Set.Make(
    (struct
      type t = string
      let compare : t -> t -> int = Stdlib.compare
    end)
  )

  type graph_working = {
    nodes : Node_set.t;
    attr : id option Attr.M.t;
    parent : string option;
  }

  let create_graph_and_clusters dot =
    (* pass 1*)

    (* collect node attributes *)
    let def_node_attr = ref Attr.empty in
    let node_attr = Hashtbl.create 97 in

    (* collect cluster attributes *)
    let def_clust_attr = ref Attr.empty in
    let clust_attr = Hashtbl.create 97 in

    (* collect clusters nodes *)
    let graph_hash = Hashtbl.create 97 in

    let add_node_attr id al =
      let l = try Hashtbl.find node_attr id
        with Not_found -> !def_node_attr in
      Hashtbl.replace node_attr id (Attr.addl l al) in

    let add_clust_attr id_opt al =
      match id_opt with
      | Some id ->
        let s = get_string id in
        let l = try Hashtbl.find clust_attr s
          with Not_found -> !def_clust_attr in
        Hashtbl.replace clust_attr s (Attr.addl l al)
      | _ -> () in

    let add_clust_node parent cluster_op (id_node, _) =
      let strip_cluster_prefix id = 
        let s = get_string id in
        if String.starts_with ~prefix:"cluster_" s then 
          String.sub s 8 (String.length s - 8)
        else
          s in
      let valid_cluster_id_of_cluster_id cluster_id = 
        match cluster_id with
        | Some (Some id) -> Some (Some (strip_cluster_prefix id))
        | Some None -> None
        | None -> Some None
      in
      let string_opt_of_cluster_id cluster_id = 
        match cluster_id with
        | Some (Some id) -> Some (strip_cluster_prefix id)
        | Some None -> (* UNREACHABLE *) None
        | None -> None
      in
      match valid_cluster_id_of_cluster_id cluster_op with
      | Some s_cluster ->
        begin
          let subgraph = try Hashtbl.find graph_hash s_cluster
            with Not_found -> 
              { nodes = Node_set.empty;
                attr = Attr.empty;
                parent = string_opt_of_cluster_id parent} in
          let subgraph_new = {
            subgraph with 
            nodes = Node_set.add (get_string id_node) subgraph.nodes
          } in
          Hashtbl.replace graph_hash s_cluster subgraph_new
        end
      | None -> () in

    let rec collect_attr parent cluster_op stmts =
      List.iter (
        function
        | Node_stmt (id, al) ->
          add_node_attr id al;
          add_clust_node parent cluster_op id
        | Edge_stmt (NodeId id, nl, _) ->
          add_node_attr id [];
          add_clust_node parent cluster_op id;
          List.iter (function | NodeId id -> 
                                add_node_attr id [];
                                add_clust_node parent cluster_op id
                              | _ -> ()) nl
        | Edge_stmt (NodeSub _, _, _) -> ()
        | Attr_graph al ->
          begin match cluster_op with
            | Some id -> add_clust_attr id al
            | None -> (* failwith "UNREACHABLE" *) ()
          end
        | Attr_node al -> def_node_attr := Attr.addl !def_node_attr al
        | Attr_edge _ -> ()
        | Equal (al_key, al_val) -> 
          let al = [[al_key, Some al_val]] in
          begin match cluster_op with
          | Some id -> add_clust_attr id al
          | None -> add_clust_attr None al
          end
        | Subgraph (SubgraphDef (id, stmts)) ->
          collect_attr cluster_op (Some id) stmts
        (* | Subgraph (SubgraphId _) -> () *)
        | _ -> ()
      ) stmts
    in
    collect_attr None None dot.stmts;

    (* pass 2: build the graph and the clusters *)
    let def_edge_attr = ref Attr.empty in
    let nodes = Hashtbl.create 97 in
    let node g id _ =
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
             fst (List.fold_left
                    (fun (g,pvn) m -> match m with
                       | NodeId idm ->
                         let g,vm = node g idm [] in
                         let e = B.G.E.create pvn el vm in
                         ((B.add_edge_e g e),vm)
                       | NodeSub _ ->
                         (g,pvn))
                    (g,vn) nl)
           | Attr_edge al ->
             def_edge_attr := Attr.addl !def_edge_attr al; g
           | Subgraph (SubgraphDef (_, stmts)) ->
             add_stmts g stmts
           | _ -> g
        )
        g stmts in

    let graph = add_stmts (B.empty ()) dot.stmts in

    let clusters_hash =
      let h = Hashtbl.create 30 in
      Hashtbl.iter (fun k a -> Hashtbl.add h k [Attr.list a]) clust_attr;
      h in

    let graph_hash_out = 
      let h = Hashtbl.create 30 in
      let graph_of_graph_working gw : graph =
        { sg_nodes = List.of_seq (Node_set.to_seq gw.nodes);
          sg_attr = [Attr.list gw.attr];
          sg_parent = gw.parent;
        }
      in
      Hashtbl.iter (fun k gw -> Hashtbl.add h k (graph_of_graph_working gw)) graph_hash;
      h in

    graph, clusters_hash, graph_hash_out

  let get_graph_bb stmts =
    let graph_bb = ref None in
    let read_attr = function
      | (Ident "bb" , Some (String bb)) -> graph_bb := Some bb
      | _ -> () in
    let read_stmt = function
      | Attr_graph attrs -> List.iter (List.iter read_attr) attrs
      | _ -> () in
    List.iter read_stmt stmts;
    !graph_bb

  let parse_dot_from_chan c =
    let lb = Lexing.from_channel c in
    let dot =
      try
        Dot_parser.file Dot_lexer.token lb
      with Parsing.Parse_error ->
        let n = Lexing.lexeme_start lb in
        failwith (Printf.sprintf "Dot.parse: parse error character %d" n)
    in
    close_in c;
    dot

  let parse_dot f =
    let c = open_in f in
    parse_dot_from_chan c

  let parse f =
    let fst, _, _ = (create_graph_and_clusters (parse_dot f)) in
    fst

  let parse_all f =
    create_graph_and_clusters (parse_dot f)

  let parse_bounding_box_and_clusters f =
    let dot = parse_dot f in
    let graph, clusters, _  = create_graph_and_clusters dot in
    match get_graph_bb dot.stmts with
    | Some bounding_box ->
      graph, bounding_box, clusters
    | None ->
      failwith "Cannot read bounding box in xdot file"

end
