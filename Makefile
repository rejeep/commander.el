CARTON ?= carton

all: test

test:
	${CARTON} exec test/commander-test

elpa:
	${CARTON} install

.PHONY:	all test
