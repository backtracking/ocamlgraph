
rapport.ps: rapport-pp.tex biblio.bib
	latex rapport-pp
	bibtex rapport-pp
	latex rapport-pp
	latex rapport-pp
	dvips rapport-pp.dvi -o $@

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
	rm -f *.ml *-pp.tex  *.log *.dvi