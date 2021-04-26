
(* Test file for Dominators.dom_tree_to_(s)nontrivial_dom *)

open Graph

module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
  end

module G = Imperative.Digraph.ConcreteLabeled(Int)(Int)

module Dominator = Dominator.Make (G)

let g = G.create ()

let () =
  G.add_edge g 1 11;
  G.add_edge g 1 2;
  G.add_edge g 1 9;
  G.add_edge g 2 3;
  G.add_edge g 2 9;
  G.add_edge g 2 10;
  G.add_edge g 3 4;
  G.add_edge g 3 5;
  G.add_edge g 4 5;
  G.add_edge g 5 6;
  G.add_edge g 6 8;
  G.add_edge g 7 6;
  G.add_edge g 8 1;
  G.add_edge g 8 10;
  G.add_edge g 9 1;
  G.add_edge g 9 7;
  G.add_edge g 9 10;
  G.add_edge g 10 1;
  G.add_edge g 10 2;
  G.add_edge g 11 1;
  G.add_edge g 11 3

let pp_comma p () = Format.(pp_print_char p ','; pp_print_space p ())
let pp_set pf s =
  Format.(fprintf pf "@[<hv 4>{%a}@]"
            (pp_print_list ~pp_sep:pp_comma pp_print_int)
            (Dominator.S.elements s))

let () =
  let idom = Dominator.compute_idom g 1 in
  let domtree = Dominator.idom_to_dom_tree g idom in
  let s1 = Dominator.S.of_list
      (Dominator.dom_tree_to_nontrivial_dom 1 domtree)
  in
  let s2 = Dominator.dom_tree_to_snontrivial_dom 1 domtree in
  Format.(printf "@[<v>%a@]@." (pp_print_list pp_set) [s1; s2])

