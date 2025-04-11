
(* Issue #149 *)

module G = Graph.Persistent.Digraph.Concrete(
  struct
    type t = string
    let compare = compare
    let hash = Hashtbl.hash
    let equal = (=)
  end)

let g0 = G.empty
let g1 = G.add_edge g0 "a" "b"
let g2 = G.add_edge g1 "a" "c"
let g  = G.add_edge g2 "c" "a"

module Topo = Graph.Topological.Make_stable(G)

let l = Topo.fold (fun v l -> v :: l) g []
let () = assert (l = ["b"; "c"; "a"])
