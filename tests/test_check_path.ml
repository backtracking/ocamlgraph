
(* Test file for Path.Check *)

open Format
open Graph
open Pack.Digraph

let test n edges =
  let v = Array.init n V.create in
  let g = create () in
  let () = Array.iter (add_vertex g) v in
  let build (s,t) = add_edge g v.(s) v.(t) in
  List.iter build edges;
  let path = PathCheck.check_path (PathCheck.create g) in
  for i = 0 to n - 1 do
    let seen = Array.make n false in
    let pre v = seen.(V.label v) <- true in
    Dfs.prefix_component pre g v.(i);
    for j = 0 to n - 1 do
      assert (seen.(j) = path v.(i) v.(j))
    done
  done

let () =
  test 3 [0,1; 1,2];
  test 3 [];
  (* 1-cycle *)
  test 1 [0,0];
  (* 2-cycle *)
  test 2 [0,1; 1,0];
  test 3 [0,1; 1,0];
  (* 2-cycle with out edge *)
  test 3 [0,1; 1,0; 1,2];
  test 3 [2,0; 0,2; 0,1];
  test 3 [1,2; 2,1; 2,0];
  (* 2 loops *)
  test 5 [1,2; 2,1; 2,0; 3,4; 4,3];
  test 5 [1,2; 2,1; 2,0; 2,3; 3,4; 4,3];
  (* 2-cycle with in edge *)
  test 3 [1,2; 2,1; 0,2];
  test 3 [1,2; 2,1; 0,1];
  (* 2 cycles connected *)
  test 4 [0,1; 1,0; 2,3; 3,2; 2,1];
  test 4 [0,1; 1,0; 2,3; 3,2; 1,2];
  test 4 [0,1; 1,0; 2,3; 3,2; 1,2; 2,1];
  (* 3-cycle with in and out edges *)
  test 5 [0,1; 1,2; 2,0; 3,0; 2,4];
  (* 3 cycles in a row *)
  test 7 [0,1; 1,0; 1,2; 2,3; 3,2; 3,4; 4,5; 5,6; 6,4];
  (* 3 cycles with 2 cycles in a cycle *)
  test 7 [0,1; 1,0; 1,2; 2,3; 3,2; 3,4; 4,5; 5,6; 6,4; 5,2];
  printf "test check_path: all tests succeeded.@."
