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
(*  Author:                                                               *)
(*    - Benoit Bataille  (benoit.bataille@gmail.com)                      *)
(*                                                                        *)
(**************************************************************************)

open Graph

type cluster = string

module Make (Tree : Graphviz.GraphWithDotAttrs ) : sig

  val from_tree : [> `widget] Gtk.obj -> Tree.t -> Tree.V.t ->
    (Tree.V.t,Tree.E.t,cluster) XDot.graph_layout

end

module type Tree = sig
  type t
  module V : sig
    type t
    type label
    val label : t -> label
  end
  module E : sig
    type t
    type label
    val src : t -> V.t
    val dst : t -> V.t
  end
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_edges_e : (E.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val iter_pred : (V.t -> unit) -> t -> V.t -> unit
  val fold_succ : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  val fold_pred : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
end

module MakeFromDotModel (Tree : Tree with
type V.label = DGraphModel.DotG.V.t and type E.label = unit) : sig

  val from_model : Tree.t -> Tree.V.t -> 
    (DGraphModel.DotG.V.t, DGraphModel.DotG.E.t, cluster)
    DGraphModel.abstract_model ->
    (Tree.V.t, Tree.E.t, cluster) XDot.graph_layout

end
