
(*       0
       /   \
      /     \
     v       v
     1---2---3    (All edges are undirected,
     |\     /|     apart from 0->1 0->3 1->5 and 3->6.)
     | \   / |
     |   4   |
     |  / \  |
     v /   \ v
     5       6
*)

open Format
open Graph
open Pack.Digraph

let debug = false

let g = create ()
let v = Array.init 7 V.create
let () = Array.iter (add_vertex g) v
let adde x y = add_edge g v.(x) v.(y)
let addu x y = adde x y; adde y x
let () = adde 0 1; adde 0 3
let () = addu 1 2; addu 2 3
let () = adde 1 5; adde 3 6
let () = addu 1 4; addu 4 3; addu 5 4; addu 4 6

module B = Traverse.Bfs(Pack.Digraph)

let dist = Array.make 7 (-1)
let reset () = Array.fill dist 0 7 (-1)
let mark v d =
  let i = V.label v in
  (* eprintf "visit %d at distance %d@." i d; *)
  assert (dist.(i) = -1); (* visit at most once *)
  dist.(i) <- d

let test s dl =
  reset ();
  B.iter_component_dist mark g v.(s);
  List.iter (fun (d, vl) ->
  List.iter (fun v -> assert (dist.(v) = d)) vl) dl

let () = test 0 [0, [0];
                 1, [1;3];
                 2, [2;4;5;6]; ]
let () = test 2 [-1, [0];
                 0, [2];
                 1, [1;3];
                 2, [4;5;6]; ]
let () = test 5 [-1, [0];
                 0, [5];
                 1, [4];
                 2, [1;3;6];
                 3, [2]; ]

let () = printf "All tests succeeded.@."
