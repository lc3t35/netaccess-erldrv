
ERLC = erlc
ERL = erl
ERLCFLAGS = -W -v
DEBUGFLAGS = +debug_info

INCLUDES = -I ../../include

%.beam:%.erl
	${ERLC} ${ERLCFLAGS} ${INCLUDES} ${DEBUGFLAGS} $<

%.boot:%.rel %.app
	${ERLC} -pa ../../ebin $<

%.tar.gz:	%.boot
	${ERL} -noshell -pa ../../ebin -run systools make_tar $* -run init stop

all: lapd 

lapd:	lapd_app.beam lapd_sup.beam lapd_fsm.beam

release: lapd_data.tar.gz lapd sys.config 

clean:
	-rm lapd_app.beam lapd_sup.beam lapd_fsm.beam \
			lapd_data.script lapd_data.tar.gz
