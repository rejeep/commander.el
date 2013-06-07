ECUKES = $(shell find elpa/ecukes-*/bin/ecukes | tail -1)

all:
	~/.carton/bin/carton exec ${ECUKES} features
