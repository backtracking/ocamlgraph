
rapport.ps: rapport.tex biblio.bib
	latex rapport
	bibtex rapport
	latex rapport
	latex rapport
	dvips rapport.dvi -o $@

%-pp.tex: pp.ml %.tex	 
	ocaml $^ > $@

pp.ml: pp.mll
	ocamllex pp.mll

print: rapport.ps
	lpr $^
