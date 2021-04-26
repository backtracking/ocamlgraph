
(* Test file for Connectivity.sstrong_articulation_points *)

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
  let hash = Hashtbl.hash
  let equal = (=)
  let default = "X"
  end

module GI = Imperative.Digraph.Concrete(Int)
module CI = Components.Connectivity(Builder.I(GI))

module GS = Persistent.Digraph.ConcreteBidirectional(String)
module CS = Components.BiConnectivity(GS)

(* From Fig. 1, Italiano, Laura, and Santaroni, "Finding strong bridges and
   strong articulation points in linear time", TCS 447(2012). *)
let g1 = GI.create ()
let () =
  GI.add_edge g1 1 2;
  GI.add_edge g1 1 3;
  GI.add_edge g1 2 3;
  GI.add_edge g1 3 1;
  GI.add_edge g1 3 4;
  GI.add_edge g1 3 6;
  GI.add_edge g1 3 7;
  GI.add_edge g1 4 5;
  GI.add_edge g1 5 1;
  GI.add_edge g1 5 2;
  GI.add_edge g1 5 4;
  GI.add_edge g1 5 7;
  GI.add_edge g1 6 5;
  GI.add_edge g1 6 7;
  GI.add_edge g1 7 6;
  GI.add_edge g1 7 5

(* From Fig. 2, Italiano, Laura, and Santaroni, "Finding strong bridges and
   strong articulation points in linear time", TCS 447(2012). *)
let g2 = GI.create ()
let () =
  GI.add_edge g2 1 2;
  GI.add_edge g2 2 3;
  GI.add_edge g2 3 4;
  GI.add_edge g2 4 5;
  GI.add_edge g2 5 6;
  GI.add_edge g2 6 1

(* From Fig. 7, Italiano, Laura, and Santaroni, "Finding strong bridges and
   strong articulation points in linear time", TCS 447(2012). *)
let g3 =
  List.fold_left (fun g (src, dst) -> GS.add_edge g src dst) GS.empty
    [("A", "B");
     ("B", "A");
     ("B", "E");
     ("C", "A");
     ("C", "B");
     ("C", "D");
     ("C", "E");
     ("D", "C");
     ("D", "E");
     ("E", "C");
     ("E", "D");
     ("E", "J");
     ("E", "I");
     ("F", "D");
     ("F", "J");
     ("H", "F");
     ("H", "I");
     ("I", "H");
     ("I", "J");
     ("I", "E");
     ("J", "E");
     ("J", "F");
     ("J", "I") ]

(* From Fig. 3, Jaberi, "On computing the 2-vertex-connected components of
   directed graphs", Discrete Applied Mathematicds 204(2016). *)
let g4 = GI.create ()
let () =
  GI.add_edge g4 1 2;
  GI.add_edge g4 2 3;
  GI.add_edge g4 3 5;
  GI.add_edge g4 3 9;
  GI.add_edge g4 3 11;
  GI.add_edge g4 3 12;
  GI.add_edge g4 4 1;
  GI.add_edge g4 4 5;
  GI.add_edge g4 4 7;
  GI.add_edge g4 4 9;
  GI.add_edge g4 5 3;
  GI.add_edge g4 5 4;
  GI.add_edge g4 6 7;
  GI.add_edge g4 6 9;
  GI.add_edge g4 7 6;
  GI.add_edge g4 7 8;
  GI.add_edge g4 7 9;
  GI.add_edge g4 8 6;
  GI.add_edge g4 9 3;
  GI.add_edge g4 9 4;
  GI.add_edge g4 9 6;
  GI.add_edge g4 9 7;
  GI.add_edge g4 10 3;
  GI.add_edge g4 10 11;
  GI.add_edge g4 11 10;
  GI.add_edge g4 11 12;
  GI.add_edge g4 12 3;
  GI.add_edge g4 12 10;
  GI.add_edge g4 12 13;
  GI.add_edge g4 13 14;
  GI.add_edge g4 14 3;
  GI.add_edge g4 14 10

(* From Fig. 4, Jaberi, "On computing the 2-vertex-connected components of
   directed graphs", Discrete Applied Mathematicds 204(2016). *)
let g5 = GI.create ()
let () =
  GI.add_edge g5 0 1;
  GI.add_edge g5 0 2;
  GI.add_edge g5 1 0;
  GI.add_edge g5 1 2;
  GI.add_edge g5 2 0;
  GI.add_edge g5 2 1;
  GI.add_edge g5 2 3;
  GI.add_edge g5 2 4;
  GI.add_edge g5 2 5;
  GI.add_edge g5 3 2;
  GI.add_edge g5 3 4;
  GI.add_edge g5 3 5;
  GI.add_edge g5 4 2;
  GI.add_edge g5 4 3;
  GI.add_edge g5 4 5;
  GI.add_edge g5 4 6;
  GI.add_edge g5 4 7;
  GI.add_edge g5 5 2;
  GI.add_edge g5 5 3;
  GI.add_edge g5 5 4;
  GI.add_edge g5 6 4;
  GI.add_edge g5 6 7;
  GI.add_edge g5 7 4;
  GI.add_edge g5 7 6;
  GI.add_edge g5 7 8;
  GI.add_edge g5 7 9;
  GI.add_edge g5 7 11;
  GI.add_edge g5 7 14;
  GI.add_edge g5 8 7;
  GI.add_edge g5 8 9;
  GI.add_edge g5 8 10;
  GI.add_edge g5 9 8;
  GI.add_edge g5 9 10;
  GI.add_edge g5 9 11;
  GI.add_edge g5 9 13;
  GI.add_edge g5 10 7;
  GI.add_edge g5 10 11;
  GI.add_edge g5 10 9;
  GI.add_edge g5 11 7;
  GI.add_edge g5 11 8;
  GI.add_edge g5 11 10;
  GI.add_edge g5 12 4;
  GI.add_edge g5 12 13;
  GI.add_edge g5 13 12;
  GI.add_edge g5 13 14;
  GI.add_edge g5 13 15;
  GI.add_edge g5 14 2;
  GI.add_edge g5 14 12;
  GI.add_edge g5 15 12

let pp_comma p () = Format.(pp_print_char p ','; pp_print_space p ())
let pp_set pp_ele pf s =
  Format.(fprintf pf "@[<hv 4>{%a}@]"
            (pp_print_list ~pp_sep:pp_comma pp_ele) s)

let () =
  let saps1 = CI.sstrong_articulation_points g1 in
  let saps2 = CI.sstrong_articulation_points g2 in
  let saps3 = CS.sstrong_articulation_points g3 in
  let saps4 = CI.sstrong_articulation_points g4 in
  let saps5 = CI.sstrong_articulation_points g5 in
  Format.(printf "@[<v>%a@,%a@,%a@,%a@,%a@]@."
            (pp_set pp_print_int) (CI.S.elements saps1)
            (pp_set pp_print_int) (CI.S.elements saps2)
            (pp_set pp_print_string) (CS.S.elements saps3)
            (pp_set pp_print_int) (CI.S.elements saps4)
            (pp_set pp_print_int) (CI.S.elements saps5)
         )

