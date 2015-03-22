EMACS ?= emacs
CASK ?= cask
CASK_EXEC ?= ${CASK} exec

all: test

test: clean-elc
	${MAKE} unit

unit:
	${CASK_EXEC} ${EMACS} -Q -batch -l init.el -l jmax-test.el --eval "(ert t)"

compile:
	${CASK_EXEC} ${EMACS} -Q -batch -f batch-byte-compile *.el

clean-elc:
	rm -f *.elc

.PHONY:	all test package clean-elc
