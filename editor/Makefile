tortue.opt: tortue.ml 
	ocamlopt outils_math.ml
	ocamlopt -o $@ graphics.cmxa outils_math.cmx $^


graphEd.opt : outils_math.ml outils_tort.ml graphEd.ml
	ocamlopt -o $@ graphics.cmxa -I .. ../graph.cmxa $^


graphEdGTK : outils_math.ml outils_tort.ml posGraph.ml graphEdGTK.ml
	ocamlopt -o $@ -I +lablgtk2 lablgtk.cmxa lablgnomecanvas.cmxa unix.cmxa -I .. ../graph.cmxa $^

editor.opt:
	make -C .. 

clean:
	rm -f *.opt
	rm -f *.cm[iox]