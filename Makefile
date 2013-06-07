CARTON ?= carton
ECUKES = $(shell find elpa/ecukes-*/bin/ecukes | tail -1)

all: unit ecukes

ecukes: elpa
	${CARTON} exec ${ECUKES} features

unit: elpa
	${CARTON} exec emacs -Q --script test/commander-test.el

elpa:
	${CARTON} install

.PHONY:	all ecukes unit
