# Makefile of the bootstrapping of the type annotation of Scheme+ for Guile
# author: Damien Mattei

# annot+.scm of this directory is the source of the type annotation of the parser , written in Scheme+ (curly infix) ;
# ../annot+.scm , the module (SRFI-105 annot+) used by the parser , is generated from it by the parser of Scheme+ for
# Guile (curly-infix2prefix4guile.scm) , which uses the module (SRFI-105 annot+) already installed : the bootstrapping
# (as in SRFI 105 for Racket , bootstrapping/annot+.rkt and annot+.rkt).
#
# make         : parse annot+.scm in the subdirectory MODULE_DIRECTORY
# make diff    : compare the parsed file with the installed one
# make install : install the parsed file in ../annot+.scm and remove the compiled files of Guile (Guile does not
#                recompile a module when a module it imports has changed , and the type tables of init-annot-data , a
#                macro , are copied in the compiled code of the modules that use it)
# make clean   : remove the parsed files
#
# after make install , a new make must give the same file (the installed module parses its own source the same way)


SHELL := /bin/bash

# path to the Scheme+ for Guile directory (the one of Scheme+.scm , Scheme+/ , SRFI-105/ and the parsers)
SCHEME_PLUS=../..

SCHEME_EXEC=guile

# the parser of Scheme+ for Guile
PARSER:=$(SCHEME_EXEC) -L $(SCHEME_PLUS) -e main -s $(SCHEME_PLUS)/curly-infix2prefix4guile.scm

# sub directory where the parsed file will be generated
MODULE_DIRECTORY=parsed_files_directory

# directory of the installed module
INSTALL_DIRECTORY=..

# the compiled files of Guile
GUILE_CACHE=$${XDG_CACHE_HOME:-$$HOME/.cache}/guile

OBJECT=$(MODULE_DIRECTORY)/annot+.scm


all: $(MODULE_DIRECTORY) $(OBJECT)

$(MODULE_DIRECTORY) :
	mkdir $@

# if an error occur and the file is empty it will stop further Makefile call so i remove it:
.DELETE_ON_ERROR:

$(MODULE_DIRECTORY)/annot+.scm: annot+.scm | $(MODULE_DIRECTORY)
	@echo PARSING $< :
	$(PARSER) $< > $@

diff: $(OBJECT)
	diff $(INSTALL_DIRECTORY)/annot+.scm $(OBJECT) && echo "the parsed file is the installed one"

install: $(OBJECT)
	@echo "Installing the parsed file in $(INSTALL_DIRECTORY) :"
	cp $(OBJECT) $(INSTALL_DIRECTORY)/annot+.scm
	@echo "Removing the compiled files of Guile ($(GUILE_CACHE)) :"
	rm -rf $(GUILE_CACHE)
	@echo "Done."

clean:
	rm -rf $(OBJECT)
	rm -rf $(MODULE_DIRECTORY)

.PHONY: all diff install clean
