
rapport.ps: rapport.tex biblio.bib
	latex rapport
	bibtex rapport
	latex rapport
	latex rapport
	dvips rapport.dvi -o $@

print: rapport.ps
	lpr $^
