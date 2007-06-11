
open Graph
open Ed_hyper

type visibility = Visible | BorderNode | Hidden

type node_info = { 
  label : string;
  mutable visible : visibility;
  mutable depth : int;
  mutable turtle : turtle;
}

let make_node_info s = 
  { label = s; visible = Hidden; depth = 0; turtle = dummy_turtle }

type edge_info = {
  mutable visited : bool;
  mutable edge_turtle : turtle;
  mutable edge_distance : float;
  mutable edge_steps : int;
}

let make_edge_info () =
  { visited = false; edge_turtle = dummy_turtle; 
    edge_distance = 0.; edge_steps = 0; }

module EDGE = struct
  type t = edge_info
  let compare = Pervasives.compare
  let default = make_edge_info ()
end

module G = 
  Imperative.Graph.AbstractLabeled(struct type t = node_info end)(EDGE)

module B = Builder.I(G)

module GmlParser = 
  Gml.Parse
    (B)
    (struct 
      let node l = 
	make_node_info
	  (try 
	      match List.assoc "id" l 
	      with Gml.Int n -> string_of_int n | _ -> "<no id>"
	    with Not_found -> "<no id>")
      let edge _ = make_edge_info ()
    end)

module DotParser = 
  Dot.Parse
    (B)
    (struct 
      let node (id,_) _ = match id with
	| Dot_ast.Ident s
	| Dot_ast.Number s
	| Dot_ast.String s
	| Dot_ast.Html s -> make_node_info s
      let edge _ = make_edge_info ()
    end)

let parse_file f = 
  if Filename.check_suffix f ".gml" then
    GmlParser.parse f
  else
    DotParser.parse f

module Components = Components.Make(G)
module Dfs = Traverse.Dfs(G)

(* current graph *)

let graph = ref (G.create ())

exception Choose of G.V.t

let choose_root () =
  try
    G.iter_vertex (fun v -> raise (Choose v)) !graph;
    Format.eprintf "empty graph@."; exit 0
  with Choose v ->
    v

let string_of_label x = (G.V.label x).label

let edge v w = G.mem_edge !graph v w || G.mem_edge !graph w v 

(* Parsing of the command line *)

let load_graph f =
  graph := parse_file f

let dfs = ref false

let refresh_rate = ref 10

let aa = ref true

let () = 
  Arg.parse
    ["-dfs", Arg.Set dfs, "DFS drawing strategy";
     "-bfs", Arg.Clear dfs, "BFS drawing strategy";
     "-rr", Arg.Set_int refresh_rate, "set the refresh rate, must be greater than 0";
     "-aa", Arg.Clear aa, "turn off anti-aliased mode";
    ]
    load_graph 
    "editor [options] <graph file>"

    
(* successor edges *)

module H2 = 
  Hashtbl.Make
    (struct 
      type t = G.V.t * G.V.t
      let hash (v,w) = Hashtbl.hash (G.V.hash v, G.V.hash w)
      let equal (v1,w1) (v2,w2) = G.V.equal v1 v2 && G.V.equal w1 w2 
    end)


module H = Hashtbl.Make(G.V)
