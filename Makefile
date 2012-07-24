###########################################################################
##                                                                       ##
##                               OCamlCC                                 ##
##                                                                       ##
##                    Michel Mauny, Benoit Vaugon                        ##
##                          ENSTA ParisTech                              ##
##                                                                       ##
##    This file is distributed under the terms of the CeCILL license.    ##
##    See file LICENSE-en.                                               ##
##                                                                       ##
###########################################################################

include etc/Makefile.conf

all: config
	@make --no-print-directory -C src

config:
	@if [ etc/Makefile.conf -ot VERSION -o \
             etc/Makefile.conf -ot configure ]; then \
          echo 'Configuration files are not up to date.' 1>&2; \
	  echo 'Please run `./configure` (with right options).' 1>&2; \
          exit 1; \
	fi

install: all
	mkdir -p "$(BINDIR)"
	mkdir -p "$(MAN1DIR)"
	mkdir -p "$(INCLUDEDIR)"
	cp bin/ocamlcc "$(BINDIR)/ocamlcc"
	gzip -c man/ocamlcc.1 > "$(MAN1DIR)/ocamlcc.1.gz"
	cp -R include/* "$(INCLUDEDIR)/"
	cp etc/config.h "$(INCLUDEDIR)/ocamlcc-byterun/config.h"

uninstall:
	rm -f "$(BINDIR)/ocamlcc"
	rm -f "$(MAN1DIR)/ocamlcc.1.gz"
	rm -Rf "$(INCLUDEDIR)"

etc/Makefile.conf:
	@echo "You must run ./configure before" 1>&2
	@exit 1

tests: all
	@make --no-print-directory -C tests

dist: clean
	dist/distgen

clean:
	@make --no-print-directory -C src clean
	@make --no-print-directory -C tests clean

.PHONY: all config install uninstall tests dist clean
