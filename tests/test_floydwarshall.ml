
(* Test file for Floyd Warshall inspired by test_johnson.ml E.PINEAU *)

open Printf
open Graph

module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end


module G = Imperative.Digraph.ConcreteLabeled(Int)(Int)


module W = struct
  type edge = G.E.t
  type t = int
  let weight e = G.E.label e
  let zero = 0
  let infinity = 999999
  let add = (+)
  let compare = compare
end

module F = Path.FloydWarshall(G)(W)
let test has_cycle tab =
  let g = G.create () in
  let build (s,w,t) = G.add_edge_e g (G.E.create s w t) in
  List.iter build tab;
  begin try
      let (m,p) = F.all_pairs_shortest_paths g in
      F.HVV.iter (fun (v, u) d -> Printf.printf "\n[%d -> %d : %d] " v u d;
                     List.iter (fun vs -> Printf.printf "V %d, " vs) (F.shortest_path p v u)) m;

    assert (not has_cycle)
    with
    | F.NegativeCycle -> printf "Negative cycle found \n"; assert (has_cycle)
    (*| _ -> failwith "Unknown"*)
end

let () =
  printf "Test N°1 same data as the test of Johnson's algorithm :";
  test false [1, 3, 2; 1, (-4), 5; 1, 8, 3; 2, 7, 5; 2, 1, 4;
              3, 4, 2; 4, (-5), 3;
              4, 2, 1; 5, 6, 4];
  printf "\nTest N° 2 negative cycle :\n";
  test true [1, 3, 2 ;  1, 3, 4 ; 2, 2, 1 ;  2, 2, 3 ; 2, 2, 4 ;  3, (-6), 1;
  3, 1, 4;  4, 4, 2; 4, 4, 3];
  printf "All tests succeeded.\n"
