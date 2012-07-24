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

MLS = $(wildcard *.ml)
BYTES = $(MLS:.ml=.byte)
NATIVES = $(MLS:.ml=.native)
CS = $(BYTES:.byte=.c)
S = $(CS:.c=)
EXES = $(CS:.c=.exe)

ARCH := $(shell arch)
OCAMLC := $(shell which ocamlc)
LIBDIR := $(shell $(OCAMLC) -where)
OCAMLCC = ../bin/ocamlcc
CC = gcc
CC_INCLUDES = -I ../etc -I ../include -I ../include/ocamlcc-byterun
CC_FLAGS = -O3 -Wall -fno-omit-frame-pointer -lm -ldl -lcurses -Wl,-E

TARGS = $(BYTES) $(NATIVES) $(CS) $(S) # $(EXES)

tests: $(TARGS)

check-project:
	@make --no-print-directory -C .. all

%.byte: %.ml
	ocamlc -g $< -o $@
	which ocamlclean && ocamlclean $@ -o $@ || /bin/true

%.native: %.ml
	ocamlopt $< -o $@

%.c: %.byte check-project
	$(OCAMLCC) -c -verbose -stat -arch $(ARCH) $<

%: %.c check-project
	$(CC) $< -o $@ $(CC_INCLUDES) $(CC_FLAGS)

ocamlc.c: $(OCAMLC) check-project
	$(OCAMLCC) -c -verbose -stat -arch $(ARCH) $< -o $@

flocon.byte: flocon.ml
	ocamlc -g unix.cma graphics.cma $< -o $@
	which ocamlclean && ocamlclean $@ -o $@ || /bin/true

flocon.native: flocon.ml
	ocamlopt -verbose unix.cmxa graphics.cmxa $< -o $@

flocon.c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch $(ARCH) $<

flocon: flocon.c check-project
	$(CC) $< -o $@ $(CC_INCLUDES) $(CC_FLAGS) -L $(LIBDIR) \
	  $(LIBDIR)/unix.a $(LIBDIR)/graphics.a -lgraphics -lX11 -lunix

clean:
	@rm -f *~ *.cmo *.cmi *.cmx *.o $(TARGS) ocamlc.c ocamlc ocamlc.exe

.PHONY: tests check-project clean
.PRESERVE: ocamlc.c
