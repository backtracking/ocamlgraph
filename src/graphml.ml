(******************************************************************************)
(*                                                                            *)
(*  Copyright (C) 2012 Pietro Abate <pietro.abate@pps.jussieu.fr>             *)
(*                                                                            *)
(*  This library is free software: you can redistribute it and/or modify      *)
(*  it under the terms of the GNU Lesser General Public License as            *)
(*  published by the Free Software Foundation, either version 3 of the        *)
(*  License, or (at your option) any later version.  A special linking        *)
(*  exception to the GNU Lesser General Public License applies to this        *)
(*  library, see the COPYING file for more information.                       *)
(*                                                                            *)
(******************************************************************************)

module type G = sig
  include Sig.G
  val vertex_properties : (string * string * string option) list
  val edge_properties : (string * string * string option) list
  val map_vertex : vertex -> (string * string) list
  val map_edge : edge -> (string * string) list
  val vertex_uid : vertex -> int
  val edge_uid : edge -> int
end

module type S = sig
  type t
  val fprintf : out_channel -> t -> unit
end

module Printer(G: G) = struct

  type t = G.t

  let header =
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
    xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns
     http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">"
  ;;

  let data_pp fmt (key,value) =
    Printf.fprintf fmt "<data key=\"%s\">%s</data>" key value

  let pp_type fmt t prop typ default = 
    Printf.fprintf fmt "<key id=\"%s\" for=\"%s\" attr.name=\"%s\" attr.type=\"%s\">" t prop prop typ;
    match default with
    |None -> Printf.fprintf fmt "</key>\n"
    |Some s -> begin 
      Printf.fprintf fmt "\n <default>%s</default>\n" s;
      Printf.fprintf fmt "</key>\n"
    end

  let fprintf fmt graph =

    Printf.fprintf fmt "%s\n" header;

    (* node attributed declaration *)
    List.iter
      (fun (prop,typ,default) -> pp_type fmt "node" prop typ default)
      G.vertex_properties;

    (* edge attributed declaration *)
    List.iter
      (fun (prop,typ,default) -> pp_type fmt "edge" prop typ default)
      G.edge_properties ;

    let directed = if G.is_directed then "edgedefault=\"directed\"" else "" in
    Printf.fprintf fmt "<graph id=\"G\" %s>\n" directed;

    (* vertex printer *)
    G.iter_vertex
      (fun vertex ->
	let id = G.vertex_uid vertex in
	let l = G.map_vertex vertex in
	Printf.fprintf fmt " <node id=\"n%d\">\n" id;
	List.iter (Printf.fprintf fmt "  %a\n" data_pp) l;
	Printf.fprintf fmt " </node>\n") 
      graph ;

    (* edge printer *)
    G.iter_edges_e
      (fun edge ->
	let n1 = G.vertex_uid (G.E.src edge) in
	let n2 = G.vertex_uid (G.E.dst edge) in
	let eid = G.edge_uid edge in
	let l = G.map_edge edge in
	Printf.fprintf fmt
	  " <edge id=\"e%d\" source=\"n%d\" target=\"n%d\">\n" eid n1 n2;
	List.iter (Printf.fprintf fmt "  %a\n" data_pp) l;
	Printf.fprintf fmt " </edge>\n") 
      graph ;

    Printf.fprintf fmt "</graph>\n";
    Printf.fprintf fmt "</graphml>\n"

end

(*
Local Variables:
compile-command: "make -C .."
End:
*)
