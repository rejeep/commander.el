CARTON ?= carton

all: test

test:
	${CARTON} exec emacs -Q --script test/commander-test.el

elpa:
	${CARTON} install

.PHONY:	all test
