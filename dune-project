(lang dune 2.4)

(name ocamlgraph)

(generate_opam_files true)

(package
  (name ocamlgraph)
  (synopsis "A generic graph library for OCaml")
  (depends
    ocaml
    stdlib-shims
    (dune (>= "2.0")))
  (tags (graph library algorithms "directed graph" vertice edge persistent imperative))
  (license LGPL-2.1-only)
  (authors "Sylvain Conchon" "Jean-Christophe Filliâtre" "Julien Signoles")
  (homepage "http://ocamlgraph.lri.fr/")
  (maintainers "filliatr@lri.fr")
  (source (github backtracking/ocamlgraph))
  (bug_reports "https://github.com/backtracking/ocamlgraph/issues/new"))

(package
  (name ocamlgraph_gtk)
  (synopsis "Displaying graphs using ocamlgraphs and gtk")
  (depends
    ocaml
    stdlib-shims
    lablgtk
    ocamlgraph
    (dune (>= "2.0")))
  (tags (graph library algorithms "directed graph" vertice edge persistent imperative))
  (license LGPL-2.1-only)
  (authors "Sylvain Conchon" "Jean-Christophe Filliâtre" "Julien Signoles")
  (homepage "http://ocamlgraph.lri.fr/")
  (maintainers "filliatr@lri.fr")
  (source (github backtracking/ocamlgraph))
  (bug_reports "https://github.com/backtracking/ocamlgraph/issues/new"))