
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

all: lapd_data 

lapd_data:	lapd_data_app.beam lapd_data_sup.beam lapd_data_fsm.beam

release: lapd_data.tar.gz lapd_data sys.config 

clean:
	-rm lapd_data_app.beam lapd_data_sup.beam lapd_data_fsm.beam \
			lapd_data.script lapd_data.tar.gz
