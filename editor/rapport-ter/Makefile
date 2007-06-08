
EPS= perpendiculaire.eps parallels.eps  
IMG= images/

rapport.ps: rapport.tex biblio.bib $(IMG)$(EPS)
	latex rapport
	bibtex rapport
	latex rapport
	latex rapport
	dvips rapport.dvi -o $@

rapport-pp.tex: pp.ml rapport.tex	 
	ocaml $^ > $@

pp.ml: pp.mll
	ocamllex pp.mll

images: $(IMG)$(EPS)

%.eps: %.png
	sam2p $^ EPS: $@

print: rapport.ps
	lpr $^
clean:
	rm -f *.ml *.tex  *.log *.dvi