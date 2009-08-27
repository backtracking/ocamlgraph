OCAMLMAKEFILE = OCamlMakefile

INCDIRS=../ +lablgtk2
LIBDIRS=../
LIBS=graph

OCAMLFLAGS=-dtypes
OCAMLBLDFLAGS = lablgtk.cma  gtkInit.cmo lablgnomecanvas.cma
OCAMLNLDFLAGS = lablgtk.cmxa gtkInit.cmx lablgnomecanvas.cmxa

SOURCES = dGraphModel.mli dGraphModel.ml xDotDraw.mli xDotDraw.ml xDot.mli xDot.ml dGraphViewItem.mli dGraphViewItem.ml dGraphView.mli dGraphView.ml concreteModel.mli concreteModel.ml dGraphViewer.ml
RESULT = dgraph
THREADS = yes

all: native-code byte-code


# OCAMLBLDFLAGS = graph.cmo
# OCAMLNLDFLAGS = graph.cmx

include $(OCAMLMAKEFILE)

doc: htdoc
	mv doc/$(RESULT)/html/* doc
	rm -r doc/$(RESULT)
