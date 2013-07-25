EMACS ?= emacs
CARTON ?= carton

all: test

test: clean-elc unit

test-compiled: compile unit clean-elc

unit:
	${CARTON} exec ert-runner run -l test/ert-loader.el

compile:
	${CARTON} exec ${EMACS} -Q -batch -f batch-byte-compile commander.el

clean-elc:
	rm -f commander.elc

.PHONY:	all test test-compiled unit compile clean-elc
