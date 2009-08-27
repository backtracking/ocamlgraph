open Graph
open DGraphModel

(** Concrete class functor *)

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
module Make(G:G) : S with module G = G and type cluster = string

(** { 2 Default concrete model } *)

(** Uses an abstract labeled imperative dgraph *)

module Vertex : Sig.ANY_TYPE with type t = DGraphModel.node_layout
module Edge : Sig.ORDERED_TYPE_DFT with type t = DGraphModel.edge_layout

module G : G
  with type t = Graph.Imperative.Digraph.AbstractLabeled(Vertex)(Edge).t

module Model : S with module G = G with type cluster = string

(** Creates a model from a dot file *)
exception DotError of string
val read_dot : ?cmd:string -> dot_file:string -> Model.model
