(lang dune 2.4)

(name ocamlgraph)

(generate_opam_files true)

(package
  (name ocamlgraph)
  (synopsis "A generic graph library for OCaml")
  (depends
    conf-autoconf
    stdlib-shims
    (ocamlfind :build))
  (depopts
    lablgtk
    conf-gnomecanvas)
  (tags (graph library algorithms "directed graph" vertice edge persistent imperative))
  (license LGPL-2.1-only)
  (authors "Sylvain Conchon" "Jean-Christophe Filliâtre" "Julien Signoles")
  (homepage "http://ocamlgraph.lri.fr/")
  (maintainers "filliatr@lri.fr")
  (source (github backtracking/ocamlgraph))
  (bug_reports "https://github.com/backtracking/ocamlgraph/issues/new"))