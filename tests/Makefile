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
CC_INCLUDES = -I ../etc -I ../runtime -I ../runtime/ocamlcc-byterun
CC_FLAGS = -D_FILE_OFFSET_BITS=64 -Wall -lm -ldl -lcurses

###

MLS = $(wildcard *.ml)
BYTES = $(MLS:.ml=.byte)
NATIVES = $(MLS:.ml=.native)
CS = $(BYTES:.byte=.c)
NONECS = $(BYTES:.byte=-none.c)
NXCCS = $(BYTES:.byte=-nxc.c)
ARCHCS = $(BYTES:.byte=-$(ARCH).c)
NXCARCHCS = $(BYTES:.byte=-nxc-$(ARCH).c)
TCCCS = $(BYTES:.byte=-tcc.c)
CLANGCS = $(BYTES:.byte=-clang.c)
S = $(CS:.c=)
NONES = $(NONECS:.c=)
NXCS = $(NXCCS:.c=)
ARCHS = $(ARCHCS:.c=)
NXCARCHS = $(NXCARCHCS:.c=)
TCCS = $(TCCCS:.c=)
CLANGS = $(CLANGCS:.c=)

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
TARGS = $(BYTES) $(NATIVES) $(CS) $(S) $(NONECS) $(NONES) $(NXCCS) $(NXCS) \
	 $(ARCHCS) $(ARCHS) $(NXCARCHCS) $(NXCARCHS) $(TCCCS) $(TCCS) \
	 $(CLANGCS) $(CLANGS)
else
TARGS = $(BYTES) $(NATIVES) $(CS) $(S) $(NONECS) $(NONES) $(NXCCS) $(NXCS) \
	$(TCCCS) $(TCCS) $(CLANGCS) $(CLANGS)
endif

###

tests: $(TARGS)
gens: $(CS) $(S)
nones: $(NONECS) $(NONES)
nxcs: $(NXCCS) $(NXCS)
archs: $(ARCHCS) $(ARCHS)

###

check-project:
	@make --no-print-directory -C .. all

###

ocamlc.c: $(OCAMLC) check-project
	$(OCAMLCC) -c -verbose -stat -arch GEN $< -o $@

ocamlc-clang.c: $(OCAMLC) check-project
	$(OCAMLCC) -c -verbose -stat -arch GEN $< -o $@

ocamlc-nxc.c: $(OCAMLC) check-project
	$(OCAMLCC) -c -verbose -stat -arch GEN -no-xconst $< -o $@

ocamlc-none.c: $(OCAMLC) check-project
	$(OCAMLCC) -c -verbose -stat -arch NONE $< -o $@

ocamlc-$(ARCH).c: $(OCAMLC) check-project
	$(OCAMLCC) -c -verbose -stat -arch $(ARCH) $< -o $@

ocamlc-nxc-$(ARCH).c: $(OCAMLC) check-project
	$(OCAMLCC) -c -verbose -stat -arch $(ARCH) -no-xconst $< -o $@

###

flocon.byte: flocon.ml
	$(OCAMLC) -g unix.cma graphics.cma $< -o $@

flocon.native: flocon.ml
	ocamlopt -verbose unix.cmxa graphics.cmxa $< -o $@

flocon.c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch GEN $<

flocon-nxc.c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch GEN -no-xconst $< -o $@

flocon-none.c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch NONE $< -o $@

flocon-$(ARCH).c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch $(ARCH) $< -o $@

flocon-nxc-$(ARCH).c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch $(ARCH) -no-xconst $< -o $@

floco%: floco%.c check-project
	$(CC) $< -o $@ $(CC_INCLUDES) $(CC_FLAGS) -L $(LIBDIR) \
	  -lgraphics -lX11 -lunix

###

%.byte: %.ml
	$(OCAMLC) -g $< -o $@

%.native: %.ml
	ocamlopt $< -o $@

%.c: %.byte check-project
	$(OCAMLCC) -c -verbose -stat -arch GEN $<

%-tcc.c: %.byte check-project
	$(OCAMLCC) -c -verbose -stat -arch GEN $<

%-nxc.c: %.byte check-project
	$(OCAMLCC) -c -verbose -stat -arch GEN -no-xconst $< -o $@

%-none.c: %.byte check-project
	$(OCAMLCC) -c -verbose -stat -arch NONE $< -o $@

%-$(ARCH).c: %.byte check-project
	$(OCAMLCC) -c -verbose -stat -arch $(ARCH) $< -o $@

%-nxc-$(ARCH).c: %.byte check-project
	$(OCAMLCC) -c -verbose -stat -arch $(ARCH) -no-xconst $< -o $@

%: %.c check-project
	$(CC) $< -o $@ $(CC_INCLUDES) $(CC_FLAGS) -O2

%-$(ARCH): %-$(ARCH).c check-project
	$(CC) $< -o $@ $(CC_INCLUDES) $(CC_FLAGS) -fno-omit-frame-pointer -O3

%-tcc: %.c check-project
	tcc $< -o $@ $(CC_INCLUDES) $(CC_FLAGS) -O3

%-clang: %-clang.c check-project
	clang $< -o $@ $(CC_INCLUDES) $(CC_FLAGS) -O3

###

clean:
	@rm -f *~ *.cmo *.cmi *.cmx *.o $(TARGS) ocamlc.c ocamlc ocamlc-nxc.c \
	  ocamlc-nxc ocamlc-$(ARCH).c ocamlc-$(ARCH) ocamlc-nxc-$(ARCH).c \
	  ocamlc-nxc-$(ARCH) ocamlc-none.c ocamlc-none ocamlc-tcc.c ocamlc-tcc \
	  ocamlc-clang.c ocamlc-clang a.out

###

.PHONY: tests check-project clean
.PRESERVE: $(TARGS)
