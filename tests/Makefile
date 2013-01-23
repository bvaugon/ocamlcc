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
OCAMLOPT := $(shell which ocamlopt)
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
TRYCS = $(BYTES:.byte=-try.c)
TCCCS = $(BYTES:.byte=-tcc.c)
CLANGCS = $(BYTES:.byte=-clang.c)
GSPCS = $(BYTES:.byte=-gsp.c)
RSPCS = $(BYTES:.byte=-rsp.c)

S = $(CS:.c=)
NONES = $(NONECS:.c=)
NXCS = $(NXCCS:.c=)
ARCHS = $(ARCHCS:.c=)
NXCARCHS = $(NXCARCHCS:.c=)
TRYS = $(TRYCS:.c=)
TCCS = $(TCCCS:.c=)
CLANGS = $(CLANGCS:.c=)
GSPS = $(GSPCS:.c=)
RSPS = $(RSPCS:.c=)

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
	 $(ARCHCS) $(ARCHS) $(NXCARCHCS) $(NXCARCHS) $(TRYCS) $(TCCCS) $(TCCS) \
	 $(CLANGCS) $(CLANGS) $(GSPCS) $(GSPS) $(RSPCS) $(RSPS)
else
TARGS = $(BYTES) $(NATIVES) $(CS) $(S) $(NONECS) $(NONES) $(NXCCS) $(NXCS) \
	 $(TCCCS) $(TCCS) $(CLANGCS) $(CLANGS) $(GSPCS) $(GSPS) $(TRYCS)
endif

###

tests: $(TARGS)
gens: $(CS) $(S)
nones: $(NONECS) $(NONES)
nxcs: $(NXCCS) $(NXCS)
archs: $(ARCHCS) $(ARCHS)
trys: $(TRYCS) $(TRYS)
tccs: $(TCCCS) $(TCCS)
clangs: $(CLANGCS) $(CLANGS)
gsps: $(GSPCS) $(GSPS)
rsps: $(RSPCS) $(RSPS)

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

ocamlc-try.c: $(OCAMLC) check-project
	$(OCAMLCC) -c -verbose -stat -exception T -no-xconst $< -o $@

ocamlc-gsp.c: $(OCAMLC) check-project
	$(OCAMLCC) -c -verbose -stat -arch GEN -stack-pointer G $< -o $@

ocamlc-rsp.c: $(OCAMLC) check-project
	$(OCAMLCC) -c -verbose -stat -arch GEN -stack-pointer R $< -o $@

###

ocamlopt.c: $(OCAMLOPT) check-project
	$(OCAMLCC) -c -verbose -stat -arch GEN $< -o $@

ocamlopt-clang.c: $(OCAMLOPT) check-project
	$(OCAMLCC) -c -verbose -stat -arch GEN $< -o $@

ocamlopt-nxc.c: $(OCAMLOPT) check-project
	$(OCAMLCC) -c -verbose -stat -arch GEN -no-xconst $< -o $@

ocamlopt-none.c: $(OCAMLOPT) check-project
	$(OCAMLCC) -c -verbose -stat -arch NONE $< -o $@

ocamlopt-$(ARCH).c: $(OCAMLOPT) check-project
	$(OCAMLCC) -c -verbose -stat -arch $(ARCH) $< -o $@

ocamlopt-nxc-$(ARCH).c: $(OCAMLOPT) check-project
	$(OCAMLCC) -c -verbose -stat -arch $(ARCH) -no-xconst $< -o $@

ocamlopt-try.c: $(OCAMLOPT) check-project
	$(OCAMLCC) -c -verbose -stat -exception T $< -o $@

ocamlopt-gsp.c: $(OCAMLOPT) check-project
	$(OCAMLCC) -c -verbose -stat -arch GEN -stack-pointer G $< -o $@

ocamlopt-rsp.c: $(OCAMLOPT) check-project
	$(OCAMLCC) -c -verbose -stat -arch GEN -stack-pointer R $< -o $@

###

flocon.byte: flocon.ml
	$(OCAMLC) -g unix.cma graphics.cma $< -o $@

flocon.native: flocon.ml
	$(OCAMLOPT) -verbose unix.cmxa graphics.cmxa $< -o $@

flocon.c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch GEN $<

flocon-nxc.c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch GEN -no-xconst $< -o $@

flocon-none.c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch NONE $< -o $@

flocon-$(ARCH).c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch $(ARCH) $< -o $@

flocon-try.c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -exception T $< -o $@

flocon-nxc-$(ARCH).c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch $(ARCH) -no-xconst $< -o $@

flocon-gsp.c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch GEN -stack-pointer G $< -o $@

flocon-rsp.c: flocon.byte check-project
	$(OCAMLCC) -signal R -c -verbose -stat -arch GEN -stack-pointer R $< -o $@

floco%: floco%.c check-project
	$(CC) $< -o $@ $(CC_INCLUDES) $(CC_FLAGS) -L $(LIBDIR) \
	  -lgraphics -lX11 -lunix

###

%.byte: %.ml
	$(OCAMLC) -g $< -o $@

%.native: %.ml
	$(OCAMLOPT) $< -o $@

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

%-try.c: %.byte check-project
	$(OCAMLCC) -c -verbose -stat -exception T -no-xconst $< -o $@

%-gsp.c: %.byte check-project
	$(OCAMLCC) -c -verbose -stat -arch GEN -stack-pointer G $< -o $@

%-rsp.c: %.byte check-project
	$(OCAMLCC) -c -verbose -stat -arch GEN -stack-pointer R $< -o $@

###

%: %.c check-project
	$(CC) $< -o $@ $(CC_INCLUDES) $(CC_FLAGS) -O3

%-$(ARCH): %-$(ARCH).c check-project
	$(CC) $< -o $@ $(CC_INCLUDES) $(CC_FLAGS) -fno-omit-frame-pointer -O3

%-try: %-try.c check-project
	g++ $< -o $@ $(CC_INCLUDES) $(CC_FLAGS) -O3

%-tcc: %.c check-project
	tcc $< -o $@ $(CC_INCLUDES) $(CC_FLAGS) -O3

%-clang: %-clang.c check-project
	clang $< -o $@ $(CC_INCLUDES) $(CC_FLAGS) -O3

###

clean:
	@/bin/rm -f *~ *.cmo *.cmi *.cmx *.o $(TARGS) a.out \
 ocaml{c,opt}{,-nxc,-$(ARCH),-nxc-$(ARCH),-try,-none,-tcc,-clang,-gsp,-rsp}{.c,}

###

.PHONY: tests check-project clean
.PRESERVE: $(TARGS)
