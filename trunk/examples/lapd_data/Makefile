
ERLC = erlc
ERLCFLAGS = -W -v
DEBUGFLAGS = +debug_info

INCLUDES = -I ../../include

%.beam:%.erl
	${ERLC} ${ERLCFLAGS} ${INCLUDES} ${DEBUGFLAGS} $<

all: lapd 

lapd:	lapd_app.beam lapd_sup.beam lapd_fsm.beam

clean:
	-rm *.beam
