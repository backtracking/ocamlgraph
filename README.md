# OCamlgraph

OCamlgraph is a graph library for OCaml. Its contribution is three-fold:

1. It provides an easy-to-use graph implementation together with several
   operations and algorithms over graphs, in [`Graph.Pack.Digraph`](https://backtracking.github.io/ocamlgraph/ocamlgraph/Graph/Pack/Digraph/index.html).
   It is a reasonably efficient imperative data structure for directed graphs
   with vertices and edges labeled with integers.

   Have a look at this module first in order to get an overview of what
   this library provides. See also [`demo.ml`](https://github.com/backtracking/ocamlgraph/blob/master/examples/demo.ml).

2. Then OCamlgraph provides several other graph implementations for those
   not satisfied with the one above. Some are persistent (immutable) and other
   imperative (mutable). Some are directed and other are not.
   Some have labels for vertices, or labels for edges, or both.
   Some have abstract types for vertices. etc.

   See interface [`Sig`](https://backtracking.github.io/ocamlgraph/ocamlgraph/Graph/Sig/index.html)
   for the graph signatures and modules [`Persistent`](https://backtracking.github.io/ocamlgraph/ocamlgraph/Graph/Persistent/index.html) and
   [`Imperative`](https://backtracking.github.io/ocamlgraph/ocamlgraph/Graph/Imperative/index.html) for the implementations.

   These implementations are written as functors: you give the types of
   vertices labels, edge labels, etc. and you get the data structure as a
   result.

4. Finally, OCamlgraph provides several classic operations and algorithms
   over graphs. They are also written as functors i.e. independently of the
   data structure for graphs. One consequence is that you can define your own
   data structure for graphs and yet re-use all the algorithms from this
   library -- you only need to provide a few operations such as iterating over
   all vertices, over the successors of a vertex, etc.


## Documentation

https://backtracking.github.io/ocamlgraph/


## Examples

You'll find examples of OCamlgraph use in subdirectory examples/
(demo.ml, demo_planar.ml, color.ml, etc.).


## Bug reports

https://github.com/backtracking/ocamlgraph/issues

