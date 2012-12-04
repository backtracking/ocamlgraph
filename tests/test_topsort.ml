
(* Test file for topological sort *)

open Printf
open Graph
open Pack.Digraph

let test n edges =
  let v = Array.init n V.create in
  let g = create () in
  let () = Array.iter (add_vertex g) v in
  let build (s,t) = add_edge g v.(s) v.(t) in
  List.iter build edges;
  display_with_gv g;
  (* run top sort *)
  let num = Array.init n 0 in
  let i = ref 0 in
  Topological.iter (fun v -> incr i; num.(v) <- !i) g;
  (* check *)
  let path = PathCheck.check_path (PathCheck.create g) in
  let check (x,y) =
    assert (num.(x) > 0 && num.(y) > 0);
    assert (num.(x) >= num.(y) || path x y || not (path y x)) in
  List.iter check edges

let () =
  test 3 [0,1; 1,2];
  printf "All test succeeded.\n"
