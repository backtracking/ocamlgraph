
(* Test file for Cycles module *)

open Graph

module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end

let pp_comma p () = Format.(pp_print_char p ','; pp_print_space p ())
let pp_edge p (s, d) = Format.fprintf p "%d -> %d" s d

module GP = Persistent.Digraph.Concrete(Int)

module GPDFS = Traverse.Dfs (GP)

let pp_has_cycles p g =
  if GPDFS.has_cycle g
  then Format.pp_print_string p "cycles"
  else Format.pp_print_string p "no cycles"

module FW = Cycles.Fashwo(struct
    include Builder.P(GP)
    let weight _ = Cycles.Normal 1
  end)

(* Eades and Linh, "A Heuristic for the Feedback Arc Set Problem", Fig. 1 *)
let g1 =
  List.fold_left (fun g (s, d) -> GP.add_edge g s d) GP.empty
    [ (1, 4);
      (1, 3);
      (2, 1);
      (2, 4);
      (3, 2);
      (4, 3);
    ]
let cycles1 = FW.feedback_arc_set g1
let g1' = List.fold_left (fun g (s, d) -> GP.remove_edge g s d) g1 cycles1

let () =
  Format.(printf "cycles1 = @[<hv 2>{ %a }@] (%a to %a)@."
    (pp_print_list ~pp_sep:pp_comma pp_edge) cycles1
    pp_has_cycles g1
    pp_has_cycles g1')

(* Eades and Linh, "A Heuristic for the Feedback Arc Set Problem", Fig. 5 *)
let g2 =
  List.fold_left (fun g (s, d) -> GP.add_edge g s d) GP.empty
    [ (1, 2);
      (1, 4);
      (2, 3);
      (2, 4);
      (3, 1);
      (4, 8);
      (5, 3);
      (5, 6);
      (6, 7);
      (7, 5);
      (8, 6);
      (8, 7);
    ]
let cycles2 = FW.feedback_arc_set g2
let g2' = List.fold_left
            (fun g (s, d) -> GP.add_edge (GP.remove_edge g s d) d s)
            g2 cycles2

let () =
  Format.(printf "cycles2 = @[<hv 2>{ %a }@] (%a to %a)@."
    (pp_print_list ~pp_sep:pp_comma pp_edge) cycles2
    pp_has_cycles g2
    pp_has_cycles g2')

(* Eades and Linh, "A Heuristic for the Feedback Arc Set Problem", Fig. 6 *)
let g3 =
  List.fold_left (fun g (s, d) -> GP.add_edge g s d) GP.empty
    [ (1, 2);
      (1, 5);
      (2, 6);
      (3, 1);
      (4, 2);
      (4, 3);
      (5, 3);
      (5, 6);
      (6, 4);
    ]
let cycles3 = FW.feedback_arc_set g3
let g3' = List.fold_left
            (fun g (s, d) -> GP.add_edge (GP.remove_edge g s d) d s)
            g3 cycles3

let () =
  Format.(printf "cycles3 = @[<hv 2>{ %a }@] (%a to %a)@."
    (pp_print_list ~pp_sep:pp_comma pp_edge) cycles3
    pp_has_cycles g3
    pp_has_cycles g3')

