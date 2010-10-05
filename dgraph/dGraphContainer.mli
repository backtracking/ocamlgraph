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

type cluster = string

type status = Global | Tree | Both

(* ABSTRACT CLASS *)
class type ['vertex, 'edge, 'cluster,
  'tree_vertex, 'tree_edge, 'tree_cluster] abstract_view_container = object
  method content : GPack.paned
  method global_view :
    ('vertex, 'edge, 'cluster) DGraphView.view option
  method tree_view :
    ('tree_vertex, 'tree_edge, 'tree_cluster) DGraphView.view option
  method set_tree_view : 'vertex -> unit
  method depth_backward : int
  method depth_forward : int
  method set_depth_backward : int -> unit
  method set_depth_forward : int -> unit
  method status : status
  method switch : status -> unit
end

module Make ( G : Graphviz.GraphWithDotAttrs ) : sig

  module TreeModel : sig

    module Tree : Sig.G 
      with type t = Graph.Imperative.Digraph.Abstract(G.V).t
      and type V.label = G.V.t

  end

  class view_container : 
    (
      (G.V.t, G.E.t, cluster) DGraphModel.abstract_model ->
      (G.V.t, G.E.t, cluster) DGraphView.view
    ) -> (
      (TreeModel.Tree.V.t, TreeModel.Tree.E.t, cluster) 
      DGraphModel.abstract_model ->
      (TreeModel.Tree.V.t, TreeModel.Tree.E.t, cluster) DGraphView.view
    ) -> 
    G.vertex -> G.t ->
    [G.vertex, G.edge, cluster,
    TreeModel.Tree.V.t, TreeModel.Tree.E.t, cluster] 
    abstract_view_container

  val from_graph :
    ?global_view:(
      (G.V.t, G.E.t, cluster) DGraphModel.abstract_model ->
      (G.V.t, G.E.t, cluster) DGraphView.view
    ) -> ?tree_view:(
      (TreeModel.Tree.V.t, TreeModel.Tree.E.t, cluster) 
      DGraphModel.abstract_model ->
      (TreeModel.Tree.V.t, TreeModel.Tree.E.t, cluster) DGraphView.view
    ) -> ?vertex:(G.vertex option) -> G.t -> view_container

  val from_graph_with_commands :
    ?packing:(GObj.widget -> unit) ->
    ?global_view:(
      (G.V.t, G.E.t, cluster) DGraphModel.abstract_model ->
      (G.V.t, G.E.t, cluster) DGraphView.view
    ) -> ?tree_view:(
      (TreeModel.Tree.V.t, TreeModel.Tree.E.t, cluster) 
      DGraphModel.abstract_model ->
      (TreeModel.Tree.V.t, TreeModel.Tree.E.t, cluster) DGraphView.view
    ) -> ?vertex:(G.vertex option) -> G.t -> GPack.table

end

module DotMake : sig

  module TreeModel : sig
    module Tree : Sig.G with type V.label = DGraphModel.DotG.V.t
  end
 
  class dot_view_container : 
    (
      (DGraphModel.DotG.V.t, DGraphModel.DotG.E.t, cluster) 
      DGraphModel.abstract_model ->
      (DGraphModel.DotG.V.t, DGraphModel.DotG.E.t, cluster) DGraphView.view
    ) -> (
      (TreeModel.Tree.V.t, TreeModel.Tree.E.t, cluster) 
      DGraphModel.abstract_model ->
      (TreeModel.Tree.V.t, TreeModel.Tree.E.t, cluster) DGraphView.view
    ) -> 
    string ->
    [DGraphModel.DotG.vertex, DGraphModel.DotG.edge, cluster,
    TreeModel.Tree.V.t, TreeModel.Tree.E.t, cluster] 
    abstract_view_container

  val from_dot :
    ?global_view:(
      (DGraphModel.DotG.V.t, DGraphModel.DotG.E.t, cluster)
      DGraphModel.abstract_model ->
      (DGraphModel.DotG.V.t, DGraphModel.DotG.E.t, cluster) DGraphView.view
    ) -> ?tree_view:(
      (TreeModel.Tree.V.t, TreeModel.Tree.E.t, cluster) 
      DGraphModel.abstract_model ->
      (TreeModel.Tree.V.t, TreeModel.Tree.E.t, cluster) DGraphView.view
    ) -> string -> dot_view_container

  val from_dot_with_commands :
    ?packing:(GObj.widget -> unit) ->
    ?global_view:(
      (DGraphModel.DotG.V.t, DGraphModel.DotG.E.t, cluster)
      DGraphModel.abstract_model ->
      (DGraphModel.DotG.V.t, DGraphModel.DotG.E.t, cluster) DGraphView.view
    ) -> ?tree_view:(
      (TreeModel.Tree.V.t, TreeModel.Tree.E.t, cluster) 
      DGraphModel.abstract_model ->
      (TreeModel.Tree.V.t, TreeModel.Tree.E.t, cluster) DGraphView.view
    ) -> string -> GPack.table

end
