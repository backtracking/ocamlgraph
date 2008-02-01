
TOP = /users/demons/filliatr/www

INCLUDES = $(TOP)/macros.prehtml  $(TOP)/footer.prehtml version.prehtml
LIBS = -I $(TOP)

OCAMLGRAPHHOME = /users/www-perso/projets/ocamlgraph

ALL = index.fr.html  index.en.html

install: all
	cp -f $(ALL) $(OCAMLGRAPHHOME)

all: $(ALL)

clean::
	rm -f *~ *.fr.html *.en.html 

YAMLPP=yamlpp $(TOP)/macros.prehtml version.prehtml

version.prehtml:
	make -C .. www/version.prehtml

# RULES

.SUFFIXES: .html .prehtml .fr.html .en.html

%.fr.html: %.prehtml $(INCLUDES)
	$(YAMLPP) -l fr $< -o $@

%.html: %.prehtml $(INCLUDES)
	$(YAMLPP) $< -o $@

%.en.html: %.prehtml $(INCLUDES)
	$(YAMLPP) -l en $< -o $@

