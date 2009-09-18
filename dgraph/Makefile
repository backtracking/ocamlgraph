##########################################################################
#                                                                        #
#  This file is part of OcamlGraph.                                      #
#                                                                        #
#  Copyright (C) 2009                                                    #
#    CEA (Commissariat à l'Énergie Atomique)                             #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1, with a linking exception.                    #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the file ../LICENSE for more details.                             #
#                                                                        #
#  Authors:                                                              #
#    - Jean-Denis Koeck (jdkoeck@gmail.com)                              #
#    - Julien Signoles  (Julien.Signoles@cea.fr)                         #
#                                                                        #
##########################################################################

OCAMLMAKEFILE = OCamlMakefile

INCDIRS=../ +lablgtk2
LIBDIRS=../
LIBS=graph

OCAMLFLAGS=-dtypes
OCAMLBLDFLAGS = lablgtk.cma  gtkInit.cmo lablgnomecanvas.cma
OCAMLNLDFLAGS = lablgtk.cmxa gtkInit.cmx lablgnomecanvas.cmxa

SOURCES = xDotDraw.mli xDotDraw.ml xDot.mli xDot.ml dGraphModel.mli dGraphModel.ml dGraphViewItem.mli dGraphViewItem.ml dGraphView.mli dGraphView.ml dGraphRandModel.ml dGraphViewer.ml
RESULT = dgraph
THREADS = yes

all: native-code byte-code


# OCAMLBLDFLAGS = graph.cmo
# OCAMLNLDFLAGS = graph.cmx

include $(OCAMLMAKEFILE)

doc: htdoc
	mv doc/$(RESULT)/html/* doc
	rm -r doc/$(RESULT)
