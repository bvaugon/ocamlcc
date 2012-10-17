###########################################################################
##                                                                       ##
##                               OCamlCC                                 ##
##                                                                       ##
##                    Michel Mauny, Benoit Vaugon                        ##
##                          ENSTA ParisTech                              ##
##                                                                       ##
##    This file is distributed under the terms of the CeCILL license.    ##
##    See file ../LICENSE-en.                                            ##
##                                                                       ##
###########################################################################

include ../etc/Makefile.conf

TARG = ../bin/ocamlcc
SRCS = $(wildcard *.ml *.mli */*.ml */*.mli)
DIRS = tools,frontend,optimizer,codegen

all: $(TARG)

$(TARG): _build/ocamlcc.native
	cp $< $@

_build/ocamlcc.native: $(SRCS) config.ml
	@$(OCAMLBUILD) -Is $(DIRS) -cflag -g -lflag -g -libs unix \
	  ocamlcc.native

config.ml: ../etc/config.ml
	cp $< $@

clean:
	@$(OCAMLBUILD) -clean
	@rm -f $(TARG) config.ml

.PHONY: all clean
