# example of Makefile for Scheme+ for Guile
# generic Makefile for all Scheme+ files
# author: Damien Mattei

# this Makefile parses all the Scheme+ files (*+.scm) of the directory where the Makefile is : the files written in
# sweet-expressions (SRFI 110 , the indentation gives the parenthesis) , named *-sweet+.scm , with the parser
# sweet-infix2prefix4guile.scm , the other ones with curly-infix2prefix4guile.scm.
# the resulting parsed Scheme files are set in the subdirectory MODULE_DIRECTORY , with the same names
# (a module (matrix+) stays in a file matrix+.scm , found by Guile with -L $(MODULE_DIRECTORY)).
#
# make       : parse all the Scheme+ files
# make clean : remove the parsed files
#
# run a parsed program with the Scheme+ of this directory first (before an older version installed in the site
# directory of Guile) and the parsed modules , example :
# guile -L parsed_files_directory -L .. parsed_files_directory/exo_retropropagationNhidden_layers_matrix_v2_by_vectors4guile+.scm
#
# after an update of Scheme+ : make clean , and rm -rf ~/.cache/guile (Guile does not recompile a module when a module
# it imports has changed)


SHELL := /bin/bash

# path to the Scheme+ for Guile directory (the one of Scheme+.scm , Scheme+/ and SRFI-105/)
SCHEME_PLUS=..

SCHEME_EXEC=guile

# the parsers of Scheme+ for Guile : curly infix (SRFI 105) and sweet-expressions (SRFI 110)
PARSER:=$(SCHEME_EXEC) -L $(SCHEME_PLUS) -e main -s $(SCHEME_PLUS)/curly-infix2prefix4guile.scm
SWEET_PARSER:=$(SCHEME_EXEC) -L $(SCHEME_PLUS) -e main -s $(SCHEME_PLUS)/sweet-infix2prefix4guile.scm


# sub directory where parsed module files will be generated
MODULE_DIRECTORY=parsed_files_directory


# Scheme+ programs to be parsed
MODULES_NAME=$(wildcard *+.scm)

# Scheme programs to create by parsing the Scheme+ files
OBJECT=$(addprefix $(MODULE_DIRECTORY)/,$(MODULES_NAME))




# create directory, build objects
# note: parsed files have the same name than source files but are in different directories
all: $(MODULE_DIRECTORY) $(OBJECT)


# create the sub directory where parsed module files will be
$(MODULE_DIRECTORY) :
	mkdir $@

# if an error occur and the file is empty it will stop further Makefile call so i remove it:

.DELETE_ON_ERROR:

# create Scheme files (*+.scm) in MODULE_DIRECTORY by parsing Scheme+ files (*+.scm)
# the sweet-expressions (*-sweet+.scm) : this rule is chosen before the next one by make (its stem is shorter)
$(MODULE_DIRECTORY)/%-sweet+.scm: %-sweet+.scm | $(MODULE_DIRECTORY)
	@echo PARSING $< :
	$(SWEET_PARSER) $< > $@

$(MODULE_DIRECTORY)/%+.scm: %+.scm | $(MODULE_DIRECTORY)
	@echo PARSING $< :
	$(PARSER) $< > $@


clean:
	rm -rf $(OBJECT)
	rm -rf $(MODULE_DIRECTORY)

.PHONY: all clean
