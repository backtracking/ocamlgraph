(* Test file for Contraction *)

#use "topfind";;
#require "ocamlgraph";;

open Graph

module Int = struct
    type t = int
    let compare = compare
    let hash = Hashtbl.hash
    let equal = (=)
    let default = 0
  end

module String = struct
    type t = string
    let compare = compare
    let default = ""
  end

module G = Persistent.Digraph.ConcreteLabeled(Int)(String)

(* Make a persistent graph where:

              0---1---6
             /         \
            2---3---7---8
           / \
          4---5---9---10---12---11    13

   and contract edges linking even numbers.

              1---6,8
             /       /
        4,2,0---3---7
            \\
              5---9---10,12---11

*)
let g = List.fold_left (fun g -> G.add_edge_e g) (G.add_vertex G.empty 13) [
    (0, "0-1", 1); (1, "1-6", 6);
    (0, "0-2", 2); (6, "6-8", 8);
    (2, "2-3", 3); (3, "3-7", 7); (7, "7-8", 8);
    (2, "2-4", 4); (2, "2-5", 5);
    (4, "4-5", 5); (5, "5-9", 9); (9, "9-10", 10);
    (10, "10-12", 12); (12, "12-11", 11)
  ]

module C = Contraction.Make(G)

let connects_even (src, _, dst) = (src mod 2 = 0) && (dst mod 2 = 0)
let g', m = C.contract' connects_even g

module Dot = Graphviz.Dot (
    struct
      include G
      let vertex_name = string_of_int
      let graph_attributes _ = []
      let default_vertex_attributes _ = []
      let vertex_attributes _ = []
      let default_edge_attributes _ = []
      let edge_attributes (_, l, _) = [`Taillabel l]
      let get_subgraph _ = None
    end)

let _ = Dot.output_graph stdout g
let _ = Dot.output_graph stdout g'

let pp_comma fmt () = Format.fprintf fmt ",@ "
let pp_map pp_value fmt =
  C.M.iter (fun x v -> Format.(fprintf fmt "%d -> %a@\n" x pp_value v))
let pp_set fmt s =
  Format.fprintf fmt "@[<hv>{%a}@]"
      Format.(pp_print_list ~pp_sep:pp_comma pp_print_int)
      (C.S.elements s)

let make_map_to_contracted = C.M.map C.S.min_elt

let _ =
  Format.open_vbox 0;
  Format.(printf "@\n# union-find sets@\n%a@\n" (pp_map pp_set) m);
  Format.(printf "# g -> g'@\n%a@\n" (pp_map pp_print_int) (make_map_to_contracted m));
  Format.close_box ()

