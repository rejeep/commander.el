EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc unit ecukes

unit:
	${CASK} exec ert-runner

ecukes:
	${CASK} exec ecukes --script features --debug

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile commander.el

clean-elc:
	rm -f commander.elc

.PHONY:	all test unit compile clean-elc ecukes
