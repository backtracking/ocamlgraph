
(* Test file for Bellman-Ford *)

open Printf
open Graph
open Pack.Digraph

let test has_cycle spec =
  let v = Array.init 5 V.create in
  let g = create () in
  let () = Array.iter (add_vertex g) v in

  let build (s,w,t) = add_edge_e g (E.create v.(s) w v.(t)) in
  List.iter build spec;
  begin try
    let cycle = bellman_ford g v.(1) in
    let print_edge e =
      printf "%d --(%d)--> %d\n" (V.label (E.src e)) (E.label e) (V.label (E.dst e))
    in
    List.iter print_edge cycle;
    assert has_cycle
  with Not_found ->
    printf "Not found \n";
    assert (not has_cycle)
  end;
  flush stdout;
  display_with_gv g

let () =
  test true [ 0, (-3), 1; 1, 1, 2; 2, 1, 0; 1, 1, 3; 3, 1, 4; 4, 1, 0 ];
(*
  test true  [ 0, (-10), 1; 1, 1, 2; 2, 1, 0; 1, 1, 3; 3, 1, 4; 4, 1, 0 ];
  test true  [ 0, (-10), 1;          2, 1, 0; 1, 1, 3; 3, 1, 4; 4, 1, 0 ];
  test true  [ 0, (-10), 1; 1, 1, 2;          1, 1, 3; 3, 1, 4; 4, 1, 0 ];
  test true  [ 0, (-10), 1; 1, 1, 2; 2, 1, 0;          3, 1, 4; 4, 1, 0 ];
  test true  [ 0, (-10), 1; 1, 1, 2; 2, 1, 0; 1, 1, 3;          4, 1, 0 ];
  test false [              1, 1, 2; 2, 1, 0; 1, 1, 3; 3, 1, 4; 4, 1, 0 ];
  test false [ 0, (-10), 1; 1, 1, 2;          1, 1, 3; 3, 1, 4;         ]
*)
()



(* Tests for module [Nonnegative] *)

module I = struct
  type t = int
  let compare : t -> t -> int = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end
module W = struct
  type label = int
  include I
  let weight x = x
  let add = (+)
  let zero = 0
end

module G = Persistent.Digraph.ConcreteLabeled(I)(I)
module NNG = Nonnegative.Persistent(G)(W)
open NNG

let g = empty
let add s t ~weight g = add_edge_e g (E.create s weight t)

let g = add 0 1 ~weight:1 g    (* should succeed *)
let g = add 1 2 ~weight:2 g    (* should succeed *)
let g = add 2 0 ~weight:(-3) g (* should fail *)
(* etc. *)

(* let () = dump Format.pp_print_int Format.pp_print_int g *)


