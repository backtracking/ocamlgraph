
(* Check that map_vertex applies the function exactly once per vertex *)

open Graph

let () = Random.init 1597

module TestB(B: Builder.S with type G.V.label = int) = struct
  let test n =
    let v = Array.init n B.G.V.create in
    let rec make g i =
      if i = n then g else make (B.add_vertex g v.(i)) (i + 1) in
    let g = ref (make (B.empty ()) 0) in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if Random.bool () then g := B.add_edge !g v.(i) v.(j)
      done
    done;
    let counter = ref 0 in
    let f x = incr counter; x in
    let g' = B.G.map_vertex f !g in
    assert (!counter = n);
    assert (B.G.nb_vertex g' = n)

  let () =
    for n = 0 to 10 do test n done
end
module TestI(G: Sig.I with type V.label = int) = TestB(Builder.I(G))
module TestP(G: Sig.P with type V.label = int) = TestB(Builder.P(G))

module Int = struct
  type t = int let compare = Stdlib.compare let equal = (=)
  let hash x = x let default = 42
end

include TestI(Pack.Digraph)
include TestI(Pack.Graph)

(* imperative, directed *)
include TestI(Imperative.Digraph.Concrete(Int))
include TestI(Imperative.Digraph.Abstract(Int))
include TestI(Imperative.Digraph.ConcreteBidirectional(Int))
include TestI(Imperative.Digraph.ConcreteLabeled(Int)(Int))
include TestI(Imperative.Digraph.AbstractLabeled(Int)(Int))
include TestI(Imperative.Digraph.ConcreteBidirectionalLabeled(Int)(Int))
(* imperative, undirected *)
include TestI(Imperative.Graph.Concrete(Int))
include TestI(Imperative.Graph.Abstract(Int))
include TestI(Imperative.Graph.ConcreteLabeled(Int)(Int))
include TestI(Imperative.Graph.AbstractLabeled(Int)(Int))

module TestM(G: Imperative.Matrix.S) = struct
  let test n =
    let g = G.make n in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if Random.bool () then G.add_edge g i j
      done
    done;
    let counter = ref 0 in
    let f x = incr counter; x in
    let g' = G.map_vertex f g in
    assert (!counter = n);
    assert (G.nb_vertex g' = n)

  let () =
    for n = 0 to 10 do test n done
end
include TestM(Imperative.Matrix.Digraph)
include TestM(Imperative.Matrix.Graph)

(* persistent, directed *)
include TestP(Persistent.Digraph.Concrete(Int))
include TestP(Persistent.Digraph.Abstract(Int))
include TestP(Persistent.Digraph.ConcreteBidirectional(Int))
include TestP(Persistent.Digraph.ConcreteLabeled(Int)(Int))
include TestP(Persistent.Digraph.AbstractLabeled(Int)(Int))
include TestP(Persistent.Digraph.ConcreteBidirectionalLabeled(Int)(Int))
(* persistent, undirected *)
include TestP(Persistent.Graph.Concrete(Int))
include TestP(Persistent.Graph.Abstract(Int))
include TestP(Persistent.Graph.ConcreteLabeled(Int)(Int))
include TestP(Persistent.Graph.AbstractLabeled(Int)(Int))
