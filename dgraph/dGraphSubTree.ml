(**************************************************************************)
(*                                                                        *)
(*  This file is part of OcamlGraph.                                      *)
(*                                                                        *)
(*  Copyright (C) 2009-2010                                               *)
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
(*    - Julien Signoles  (Julien.Signoles@cea.fr)                         *)
(*    - Jean-Denis Koeck (jdkoeck@gmail.com)                              *)
(*    - Benoit Bataille  (benoit.bataille@gmail.com)                      *)
(*                                                                        *)
(**************************************************************************)

open Graph

module type G = sig
  type t
  module V : sig
    type t
    type label
    val label : t -> label
    val hash : t -> int
    val equal : t -> t -> bool
  end
  module E : sig
    type t
  end
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val iter_pred : (V.t -> unit) -> t -> V.t -> unit
  val find_edge : t -> V.t -> V.t -> E.t
end

module type EDGE = sig
  type t
  type label
  type vertex
  val create : vertex -> label -> vertex -> t
  val label : t -> label
  val src : t -> vertex
  val dst : t -> vertex
end

module type Tree = sig
  type t
  module V : sig
    type t
    type label
    val create : label -> t
    val label : t -> label
  end
  module E : EDGE with type vertex = V.t
  val create : ?size:int -> unit -> t
  val add_vertex : t -> V.t -> unit
  val add_edge_e : t -> E.t -> unit
end

module Manipulate
  (G : G)
  (Tree : Tree with type V.label = G.V.t and type E.label = unit) =
struct

  type tree = {
    (* The tree graph *)
    structure: Tree.t;
    (* Its root *)
    root : Tree.V.t;
    (* Give correspondance between nodes of the new tree graph and nodes of
    the original graph *)
    assoc_vertex_table: (G.V.t,Tree.V.t) Hashtbl.t;
    (* Contain nodes added in an esthetic purpose *)
    ghost_vertices: (Tree.V.t,unit) Hashtbl.t;
    (* Contain edges added in an esthetic purpose *)
    ghost_edges: (Tree.E.t,unit) Hashtbl.t;
  }

  (* Getter *)
  let get_structure t = t.structure;;
  let get_root t = t.root;;

  (** Give the list of vertices in the tree graph representing a vertex
  from the old graph *)
  let get_tree_vertices vertex tree =
    try Hashtbl.find_all tree.assoc_vertex_table vertex
    with Not_found -> assert false;;

  (** True if the vertex is not to be shown *)
  let is_ghost_node v tree = Hashtbl.mem tree.ghost_vertices v;;

  (** True if the edge is not to be shown *)
  let is_ghost_edge e tree =
    Hashtbl.mem tree.ghost_edges e;;

  (** Give the old graph vertex represented by a vertex in the tree -
  Raise Ghost_vertex if the vertex is a ghost vertex *)
  exception Ghost_node;;
  let get_graph_vertex vertex tree =
    if is_ghost_node vertex tree then
      raise Ghost_node
    else
      Tree.V.label vertex;;

  (* Explore the graph from a vertex and build a tree -
  Will be used forward and backward *)
  let build src_graph tree src_vertex tree_root backward_flag depth =

    let complete_to_depth v missing =
      let pred_vertex = ref v in
      let next_vertex = ref v in
      for i=1 to (missing-1) do
	next_vertex := Tree.V.create (Tree.V.label v);
	Hashtbl.add tree.ghost_vertices !next_vertex ();
	let new_ghost_edge =
	  if backward_flag then
	    Tree.E.create !next_vertex () !pred_vertex
	  else
	    Tree.E.create !pred_vertex () !next_vertex
	in Tree.add_edge_e tree.structure new_ghost_edge;
	Hashtbl.add tree.ghost_edges new_ghost_edge ();
	pred_vertex := !next_vertex;
      done
    in

    let has_succ = ref false in
    let vertex_visited = Hashtbl.create 100 in
    let queue = Queue.create () in
    Hashtbl.add vertex_visited src_vertex true;

      (* Initialize queue *)
    if depth!=0 then
      if backward_flag then
	G.iter_pred (function a -> Queue.add (a,tree_root,depth) queue)
	  src_graph src_vertex
      else
	G.iter_succ (function a -> Queue.add (a,tree_root,depth) queue)
	  src_graph src_vertex;
      (* Empty queue *)
    let rec empty_queue () =
      if not(Queue.is_empty queue) then begin
	let (vertex,origin_vertex,depth) = Queue.take queue in
	if depth != 0 then begin
	  let new_vertex = Tree.V.create vertex in
	  Hashtbl.add tree.assoc_vertex_table vertex new_vertex;
	  if backward_flag then begin
	    let new_edge = Tree.E.create new_vertex () origin_vertex in
	    Tree.add_edge_e tree.structure new_edge
	  end else begin
	    let new_edge = Tree.E.create origin_vertex () new_vertex in
	    Tree.add_edge_e tree.structure new_edge
	  end;
	  if not(Hashtbl.mem vertex_visited vertex) then begin
	    Hashtbl.add vertex_visited vertex true;
	    if backward_flag then
	      G.iter_pred
		(function a -> Queue.add (a,new_vertex,depth-1) queue;
		  has_succ := true) src_graph vertex
	    else
	      G.iter_succ
		(function a -> Queue.add (a,new_vertex,depth-1) queue;
		  has_succ := true) src_graph vertex;
	    if !has_succ = false then complete_to_depth new_vertex depth;
	    has_succ := false;
	  end else if depth != 1 then begin
	    if backward_flag then
	      G.iter_pred (function _ -> has_succ := true) src_graph vertex
	    else
	      G.iter_succ (function _ -> has_succ := true) src_graph vertex;
	    if !has_succ then begin
	      let ghost_vertex = Tree.V.create vertex in
	      Hashtbl.add tree.ghost_vertices ghost_vertex ();
	      let new_edge =
		if backward_flag then
		  Tree.E.create ghost_vertex () new_vertex
		else
		  Tree.E.create new_vertex () ghost_vertex
	      in Tree.add_edge_e tree.structure new_edge;
	      complete_to_depth ghost_vertex (depth-1)
	    end else
	      complete_to_depth new_vertex depth;
	    has_succ := false;
	  end
	end;
	empty_queue ()
      end
    in empty_queue ();;

  (** Build a tree graph centered on a vertex and containing its
  predecessors and successors *)
  let make src_graph src_vertex depth_forward depth_backward =
    let tree = {
      structure = Tree.create ();
      root = Tree.V.create src_vertex;
      assoc_vertex_table = Hashtbl.create 100;
      ghost_vertices = Hashtbl.create 100;
      ghost_edges = Hashtbl.create 100;
    }
    in
    Hashtbl.add tree.assoc_vertex_table src_vertex tree.root;
    Tree.add_vertex tree.structure tree.root;
    build src_graph tree src_vertex tree.root false depth_forward;
    build src_graph tree src_vertex tree.root true depth_backward;
    tree;;

end

module FromDotModel
  (Tree : Tree with type V.label = DGraphModel.DotG.V.t
	       and type E.label = unit) =
struct

  type tree = {
    (* The tree graph *)
    structure: Tree.t;
    (* Its root *)
    root : Tree.V.t;
    (* Give correspondance between nodes of the new tree graph and nodes of
    the original graph *)
    assoc_vertex_table: (DGraphModel.DotG.V.t,Tree.V.t) Hashtbl.t;
    (* Contain nodes added in an esthetic purpose *)
    ghost_vertices: (Tree.V.t,unit) Hashtbl.t;
    (* Contain edges added in an esthetic purpose *)
    ghost_edges: (Tree.E.t,unit) Hashtbl.t;
  }

  (* Getter *)
  let get_structure t = t.structure;;
  let get_root t = t.root;;

  (** Give the list of vertices in the tree graph representing a vertex
  from the old graph *)
  let get_tree_vertices vertex tree =
    try Hashtbl.find_all tree.assoc_vertex_table vertex
    with Not_found -> assert false;;


  (** True if the vertex is not to be shown *)
  let is_ghost_node v tree = Hashtbl.mem tree.ghost_vertices v;;

  (** True if the edge is not to be shown *)
  let is_ghost_edge e tree =
    Hashtbl.mem tree.ghost_edges e;;

  (** Give the old graph vertex represented by a vertex in the tree -
  Raise Ghost_vertex if the vertex is a ghost vertex *)
  exception Ghost_node;;
  let get_graph_vertex vertex tree =
    if is_ghost_node vertex tree then
      raise Ghost_node
    else
      Tree.V.label vertex;;

  (* Explore the graph from a vertex and build a tree -
  Will be used forward and backward *)
  let build model tree src_vertex tree_root backward_flag depth =

    let complete_to_depth v missing =
      let pred_vertex = ref v in
      let next_vertex = ref v in
      for i=1 to (missing-1) do
	next_vertex := Tree.V.create (Tree.V.label v);
	Hashtbl.add tree.ghost_vertices !next_vertex ();
	let new_ghost_edge =
	  if backward_flag then
	    Tree.E.create !next_vertex () !pred_vertex
	  else
	    Tree.E.create !pred_vertex () !next_vertex
	in Tree.add_edge_e tree.structure new_ghost_edge;
	Hashtbl.add tree.ghost_edges new_ghost_edge ();
	pred_vertex := !next_vertex;
      done
    in

    let has_succ = ref false in
    let vertex_visited = Hashtbl.create 100 in
    let queue = Queue.create () in
    Hashtbl.add vertex_visited src_vertex true;

    (* Initialize queue *)
    if depth!=0 then
      if backward_flag then
	model#iter_pred
	  (function a -> Queue.add (a,tree_root,depth) queue) src_vertex
      else
	model#iter_succ
	  (function a -> Queue.add (a,tree_root,depth) queue) src_vertex;
      (* Empty queue *)
    let rec empty_queue () =
      if not(Queue.is_empty queue) then begin
	let (vertex,origin_vertex,depth) = Queue.take queue in
	if depth != 0 then begin
	  let new_vertex = Tree.V.create vertex in
	  Hashtbl.add tree.assoc_vertex_table vertex new_vertex;
	  if backward_flag then begin
	    let new_edge = Tree.E.create new_vertex () origin_vertex in
	    Tree.add_edge_e tree.structure new_edge
	  end else begin
	    let new_edge = Tree.E.create origin_vertex () new_vertex in
	    Tree.add_edge_e tree.structure new_edge
	  end;
	  if Hashtbl.mem vertex_visited vertex then if depth != 1 then begin
	    if backward_flag then
	      (try
		 model#iter_pred
		   (function _ -> has_succ := true; raise Exit)
		   vertex
	       with Exit ->
		 ())
	    else
	      (try
		 model#iter_succ
		   (function _ -> has_succ := true; raise Exit)
		   vertex
	       with Exit ->
		 ());
	    if !has_succ then begin
	      let ghost_vertex = Tree.V.create vertex in
	      Hashtbl.add tree.ghost_vertices ghost_vertex ();
	      let new_edge =
		if backward_flag then
		  Tree.E.create ghost_vertex () new_vertex
		else
		  Tree.E.create new_vertex () ghost_vertex
	      in Tree.add_edge_e tree.structure new_edge;
	      complete_to_depth ghost_vertex (depth-1)
	    end else
	      complete_to_depth new_vertex depth;
	    has_succ := false;
	  end else begin
	    Hashtbl.add vertex_visited vertex true;
	    if backward_flag then
	      model#iter_pred
		(function a ->
		  Queue.add (a,new_vertex,depth-1) queue;
		  has_succ := true)
		vertex
	    else
	      model#iter_succ
		(function a ->
		  Queue.add (a,new_vertex,depth-1) queue;
		  has_succ := true)
		vertex;
	    if not !has_succ then complete_to_depth new_vertex depth;
	    has_succ := false;
	  end
	end;
	empty_queue ()
      end
    in
    empty_queue ();;

  (** Build a tree graph centered on a vertex and containing its
  predecessors and successors *)
  let make model src_vertex depth_forward depth_backward =
    let tree = {
      structure = Tree.create ();
      root = Tree.V.create src_vertex;
      assoc_vertex_table = Hashtbl.create 100;
      ghost_vertices = Hashtbl.create 100;
      ghost_edges = Hashtbl.create 100;
    } in
    Hashtbl.add tree.assoc_vertex_table src_vertex tree.root;
    Tree.add_vertex tree.structure tree.root;
    build model tree src_vertex tree.root false depth_forward;
    build model tree src_vertex tree.root true depth_backward;
    tree;;

end
