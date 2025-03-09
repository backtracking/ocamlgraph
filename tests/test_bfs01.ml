
(* Test file for Path.Bfs01 *)

open Graph
open Pack.Digraph

let zero e =
  E.label e = 0

let test n =
  let g = create () in
  let nv = 2*n+2 in
  let v = Array.init nv V.create in
  Array.iter (add_vertex g) v;
  let edge i d j = add_edge_e g (E.create v.(i) d v.(j)) in
  for i = 1 to n do let i = 2*i in edge (i-2) 0 i done; edge (2*n) 1 (2*n+1);
  edge 0 1 1; for i = 0 to n-1 do let i = 2*i+1 in edge i 1 (i+2) done;
  let check v d =
    let i = V.label v in
    assert (d = if i mod 2 = 0 then 0
                else if i = 2*n+1 then 1
                else (i+1) / 2) in
  bfs_0_1 check g ~zero v.(0)

let () =
  for n = 0 to 10 do test n done



