
rapport.ps: rapport.tex biblio.bib
	latex rapport
	bibtex rapport
	latex rapport
	latex rapport
	dvips rapport.dvi -o $@

rapport-pp.tex: pp.ml rapport.tex	 
	ocaml $^ > $@

pp.ml: pp.mll
	ocamllex pp.mll

%.eps: %.png
	pngtopnm $^
	pnmtops %.pnm
	ps2epsi %.ps
	rm *.pnm
	rm *.ps

print: rapport.ps
	lpr $^
clean:
	rm -f *.ml *.tex  *.log *.dvi