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

(** Search algorithms *)

(** Minimal graph signature.
    Compatible with {!Sig.G}. *)
module type G = sig
  type t
  module V : Sig.COMPARABLE
  type vertex = V.t
  module E : sig
    type t
    val src : t -> V.t
    val dst : t -> V.t
  end
  type edge = E.t
  val fold_succ_e: (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a

  val success: t -> vertex -> bool
end

module Path(G: G) = struct

  let rec final v = function
    | [] -> v
    | e :: _ when G.V.compare v (G.E.src e) <> 0 -> invalid_arg "final"
    | e :: path -> final (G.E.dst e) path

  let valid start path =
    try ignore (final start path); true
    with Invalid_argument _ -> false

  let solution g start path =
    try G.success g (final start path)
    with Invalid_argument _ -> false

end

module DFS(G: G) = struct

  module H = Hashtbl.Make(G.V)

  let search g start =
    let visited = H.create 128 in
    let test v = H.mem visited v || (H.add visited v (); false) in
    let rec dfs = function
      | [] ->
	  raise Not_found
      | (s, path) :: stack ->
	  if test s then
	    dfs stack
	  else if G.success g s then
	    s, List.rev path
	  else
	    dfs
	      (G.fold_succ_e
		  (fun e stack -> (G.E.dst e, e :: path) :: stack)
		  g s stack)
    in
    dfs [start, []]

end

module BFS(G: G) = struct

  module H = Hashtbl.Make(G.V)

  let search g start =
    let visited = H.create 128 in
    let push path e next =
      let v = G.E.dst e in
      if H.mem visited v then next
      else (H.add visited v (); (v, e :: path) :: next) in
    let rec loop next = function
      | [] ->
          if next = [] then raise Not_found;
          loop [] next
      | (v, path) :: _ when G.success g v ->
          v, List.rev path
      | (v, path) :: todo ->
          let next = G.fold_succ_e (push path) g v next in
          loop next todo in
    H.add visited start ();
    loop [] [start, []]

end

module IDS(G: G) = struct

  let search g start =
    let max_reached = ref false in
    let depth max =
      let rec dfs = function
	| [] -> raise Not_found
	| (_, path, s) :: _ when G.success g s -> s, List.rev path
	| (n, path, s) :: stack ->
	    dfs
	      (if n < max then
		 G.fold_succ_e
		   (fun e stack -> (n + 1, e :: path, G.E.dst e) :: stack)
		   g s stack
	       else (
		 max_reached := true;
		 stack
	      )) in
      dfs [0, [], start] in
    let rec try_depth d =
      try
	max_reached := false;
	depth d
      with Not_found ->
	if !max_reached then try_depth (d + 1) else raise Not_found
    in
    try_depth 0

end
