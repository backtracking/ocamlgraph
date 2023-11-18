
(*   0
     ^
     |
     |
     1---2---3
     |\     /|
     | \   / |
     |   4*  |      7---->8
     |  / \  |      ^     |
     v /   \ v      |     v
     5       6     10<----9
*)

open Format
open Graph
open Pack.Digraph

let debug = false

let g = create ()
let v = Array.init 11 V.create
let () = Array.iter (add_vertex g) v
let adde x y = add_edge g v.(x) v.(y)
let addu x y = adde x y; adde y x
let () = adde 1 0
let () = addu 1 2; addu 2 3
let () = adde 1 5; adde 3 6
let () = addu 1 4; addu 4 3; addu 5 4; addu 4 6
let () = adde 7 8; adde 8 9; adde 9 10; adde 10 7

let target = v.(4)

module G = struct
  include Pack.Digraph
  let success _ v =
    V.compare v target = 0
end

module P = Search.Path(G)
module D = Search.DFS(G)
module B = Search.BFS(G)

let test search s b =
  let start = v.(s) in
  try
    let f, path = search g start in
    assert b;
    assert (P.solution g start path);
    assert (V.compare f target = 0)
  with Not_found ->
    assert (not b)

let run search =
  test search 0 false;
  for i = 1 to 6 do test search i true done;
  for i = 7 to 10 do test search i false done

let () =
  for i = 1 to 6 do
    let _, path = B.search g v.(i) in
    assert (List.length path = if i = 2 then 2 else if i = 4 then 0 else 1)
  done

let () = run D.search
let () = run B.search

(*   0
     ^
     |
     |
     1---2---3
     |\   +3/|
     | \   / |
     |   4*  |      7---->8
     |  / \  |      ^     |
     v /   \ v      |     v
     5       6     10<----9
*)
module C = struct
  include Int type edge = G.E.t
  let weight e =
    let x, y = G.E.src e, G.E.dst e in
    if V.compare x v.(3) = 0 && V.compare y v.(4) = 0 then 3 else 1
end
module Di = Search.Dijkstra(G)(C)

let () =
  let check (i, di) =
    let _, path, d = Di.search g v.(i) in
    assert (List.length path = d);
    assert (d = di) in
  List.iter check [1,1; 2,2; 3,2; 4,0; 5,1; 6,1];
  let check i =
    try ignore (Di.search g v.(i)); assert false with Not_found -> () in
  List.iter check [0; 7; 8; 9; 10]

module I = Search.IDS(G)

(*       5  <-----  0 ------> 1 ------> 2 ------> 3 ----> 4
         |          |                   ^
         v          |                   |
         6          +-------------------+
*)
let () = G.clear g
let () = Array.iter (add_vertex g) v
let () = adde 0 1; adde 1 2; adde 2 3; adde 3 4; adde 0 2
let () = adde 0 5; adde 5 6

let () =
  for i = 0 to 4 do test I.search i true  done;
  for i = 5 to 6 do test I.search i false done
let () =
  let _, path = I.search g v.(0) in
  assert (List.length path = 3)

(* on a grid *)

let n = 10
let m = 10
let g, vm = Classic.grid ~n ~m
let start = vm.(0).(0)
let targeti = n-1 and targetj = 5
let target = vm.(targeti).(targetj)
let distance = targeti + targetj
(*
let () = for i = 0 to n-2 do remove_vertex g vm.(i).(m-2) done
let () = for i = 1 to n-1 do remove_vertex g vm.(i).(m-4) done
*)
module Gr = struct
  include Pack.Digraph
  let count = ref 0
  let reset () = count := 0
  let print msg = printf "%s: %d@." msg !count; reset ()
  let fold_succ_e f g v = incr count; fold_succ_e f g v
  let success _ v = V.compare v target = 0
end
module Co = struct
  include Int type edge = G.E.t
  let weight _e = 1
end
module He = struct
  let heuristic v =
    let l = G.V.label v in
    let i = l / m and j = l mod m in
    n-1-i + m-1-j
end
module Dfs = Search.DFS(Gr)
module Bfs = Search.BFS(Gr)
module Ids = Search.BFS(Gr)
module Dij = Search.Dijkstra(Gr)(Co)
module Astar = Search.Astar(Gr)(Co)(He)

let () =
  let _,path = Dfs.search g start in
  Gr.print "DFS";
  assert (List.length path = distance);
  let _,path = Bfs.search g start in
  Gr.print "BFS";
  assert (List.length path = distance);
  let _,path = Ids.search g start in
  Gr.print "IDS";
  assert (List.length path = distance);
  let _,_path,d = Dij.search g start in
  Gr.print "Dij";
  assert (List.length path = distance);
  assert (d = distance);
  let _,_path,d = Astar.search g start in
  Gr.print "A* ";
  assert (List.length path = distance);
  assert (d = distance);
  ()

let () = printf "All tests succeeded.@."
