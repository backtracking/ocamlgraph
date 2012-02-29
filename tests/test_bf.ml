
(* Test file for Bellman-Ford *)

open Printf
open Graph
open Pack.Digraph

let v = Array.init 5 V.create
let g = create ()
let () = Array.iter (add_vertex g) v

let add i l j = add_edge_e g (E.create v.(i) l v.(j))
let () = add 0 (-10) 1; add 1 1 2; add 2 1 0; add 1 1 4; add 4 1 3; add 3 1 0

let cycle = bellman_ford g v.(1)

let print_edge e =
  printf "%d --%d--> %d\n" (V.label (E.src e)) (E.label e) (V.label (E.dst e))
let () = List.iter print_edge cycle

