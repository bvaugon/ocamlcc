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

ARCH := $(shell arch)
OCAMLC := $(shell which ocamlc)
LIBDIR := $(shell $(OCAMLC) -where)
OCAMLCC = ../bin/ocamlcc
CC = gcc
CC_INCLUDES = -I ../etc -I ../include -I ../include/ocamlcc-byterun
CC_FLAGS = -O3 -Wall -fno-omit-frame-pointer -lm -ldl -lcurses -Wl,-E

###

MLS = $(wildcard *.ml)
BYTES = $(MLS:.ml=.byte)
NATIVES = $(MLS:.ml=.native)
CS = $(BYTES:.byte=.c)
NXCCS = $(BYTES:.byte=-nxc.c)
ARCHCS = $(BYTES:.byte=-$(ARCH).c)
NXCARCHCS = $(BYTES:.byte=-nxc-$(ARCH).c)
S = $(CS:.c=)
NXCS=$(NXCCS:.c=)
ARCHS=$(ARCHCS:.c=)
NXCARCHS=$(NXCARCHCS:.c=)

###

ifeq ($(ARCH), i386)
MANAGED_ARCH=true
else
ifeq ($(ARCH), i486)
MANAGED_ARCH=true
else
ifeq ($(ARCH), i586)
MANAGED_ARCH=true
else
ifeq ($(ARCH), i686)
MANAGED_ARCH=true
else
ifeq ($(ARCH), x86_64)
MANAGED_ARCH=true
else
MANAGED_ARCH=false
endif
endif
endif
endif
endif

###

ifeq ($(MANAGED_ARCH), true)
TARGS = $(BYTES) $(NATIVES) $(CS) $(S) $(NXCCS) $(NXCS) $(ARCHCS) $(ARCHS) \
	$(NXCARCHCS) $(NXCARCHS)
else
TARGS = $(BYTES) $(NATIVES) $(CS) $(S) $(NXCCS) $(NXCS)
endif

###

tests: $(TARGS)

check-project:
	@make --no-print-directory -C .. all

###

%.byte: %.ml
	ocamlc -g $< -o $@
	which ocamlclean && ocamlclean $@ -o $@ || /bin/true

%.native: %.ml
	ocamlopt $< -o $@

%.c: %.byte check-project
	$(OCAMLCC) -c -verbose -stat -arch NONE $<

%-nxc.c: %.byte check-project
	$(OCAMLCC) -c -verbose -stat -arch NONE -no-xconst $< -o $@

%-$(ARCH).c: %.byte check-project
	$(OCAMLCC) -c -verbose -stat -arch $(ARCH) $< -o $@

%-nxc-$(ARCH).c: %.byte check-project
	$(OCAMLCC) -c -verbose -stat -arch $(ARCH) -no-xconst $< -o $@

%: %.c check-project
	$(CC) $< -o $@ $(CC_INCLUDES) $(CC_FLAGS)

###

ocamlc.c: $(OCAMLC) check-project
	$(OCAMLCC) -c -verbose -stat -arch NONE $< -o $@

ocamlc-nxc.c: $(OCAMLC) check-project
	$(OCAMLCC) -c -verbose -stat -arch NONE -no-xconst $< -o $@

ocamlc-$(ARCH).c: $(OCAMLC) check-project
	$(OCAMLCC) -c -verbose -stat -arch $(ARCH) $< -o $@

ocamlc-nxc-$(ARCH).c: $(OCAMLC) check-project
	$(OCAMLCC) -c -verbose -stat -arch $(ARCH) -no-xconst $< -o $@

###

flocon.byte: flocon.ml
	ocamlc -g unix.cma graphics.cma $< -o $@
	which ocamlclean && ocamlclean $@ -o $@ || /bin/true

flocon.native: flocon.ml
	ocamlopt -verbose unix.cmxa graphics.cmxa $< -o $@

flocon.c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch NONE $<

flocon-nxc.c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch NONE -no-xconst $< -o $@

flocon-$(ARCH).c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch $(ARCH) $< -o $@

flocon-nxc-$(ARCH).c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch $(ARCH) -no-xconst $< -o $@

floco%: floco%.c check-project
	$(CC) $< -o $@ $(CC_INCLUDES) $(CC_FLAGS) -L $(LIBDIR) \
	  $(LIBDIR)/unix.a $(LIBDIR)/graphics.a -lgraphics -lX11 -lunix

###

clean:
	@rm -f *~ *.cmo *.cmi *.cmx *.o $(TARGS) ocamlc.c ocamlc ocamlc-nxc.c \
	  ocamlc-nxc ocamlc-$(ARCH).c ocamlc-$(ARCH) ocamlc-nxc-$(ARCH).c \
	  ocamlc-nxc-$(ARCH) a.out

###

.PHONY: tests check-project clean
.PRESERVE: ocamlc.c
