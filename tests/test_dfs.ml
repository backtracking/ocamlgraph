
(* Stack-based DFS is tricky to get right. See
   https://11011110.github.io/blog/2013/12/17/stack-based-graph-traversal.html

   On this graph,

         0
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

  an incorrect stack-based DFS starting from 0 would first mark 1 and 3,
  and then would not go as deep as possible in the traversal.

  In the following, we check that, whenever 2 and 4 are visited,
  then necessarily both 1 and 3 are already visited.
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

let () = assert (Dfs.has_cycle g)

let marked = Array.make 7 false
let reset () = Array.fill marked 0 7 false
let mark v =
  let i = V.label v in
  marked.(i) <- true;
  if marked.(2) && marked.(4) then assert (marked.(1) && marked.(3))

let pre  v = if debug then printf "pre  %d@." (V.label v); mark v
let post v = if debug then printf "post %d@." (V.label v)
let f v () = if debug then printf "fold %d@." (V.label v); mark v

let () = reset (); Dfs.iter ~pre ~post g
let () = reset (); Dfs.prefix pre g
let () = reset (); Dfs.postfix post g
let () = reset (); Dfs.iter_component ~pre ~post g v.(0)
let () = reset (); Dfs.prefix_component pre g v.(0)
let () = reset (); Dfs.postfix_component post g v.(0)
let () = reset (); Dfs.fold f () g
let () = reset (); Dfs.fold_component f () g v.(0)

module D = Traverse.Dfs(Pack.Digraph)

let rec visit it =
  let v = D.get it in
  mark v;
  visit (D.step it)

let () = try visit (D.start g) with Exit -> ()

let () = printf "All tests succeeded.@."
