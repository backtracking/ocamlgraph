
open Graph
open Ed_hyper

type node_info = { 
  label : string;
  mutable visible : bool;
  mutable depth : int;
  mutable turtle : turtle;
}

let make_info s = 
  { label = s; visible = false; depth = 0; turtle = dummy_turtle }

module G = Imperative.Graph.Abstract(struct type t = node_info end)

module B = Builder.I(G)

module GmlParser = 
  Gml.Parse
    (B)
    (struct 
      let node l = 
	make_info
	  (try 
	      match List.assoc "id" l 
	      with Gml.Int n -> string_of_int n | _ -> "<no id>"
	    with Not_found -> "<no id>")
      let edge _ = ()
    end)

module DotParser = 
  Dot.Parse
    (B)
    (struct 
      let node (id,_) _ = match id with
	| Dot_ast.Ident s
	| Dot_ast.Number s
	| Dot_ast.String s
	| Dot_ast.Html s -> make_info s
      let edge _ = ()
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

let () = 
  Arg.parse
    []
    load_graph 
    "editor [options] <graph file>"

    
