## Process this file with automake to produce Makefile.in

beamdir = $(datadir)/ebin
beam_DATA = ../ebin/netaccess_app.beam ../ebin/netaccess.beam \
		../ebin/pridrv.beam ../ebin/iisdn.beam \
		../ebin/netaccess_server.beam ../ebin/netaccess_fsm.beam

CLEANFILES = $(beam_DATA)

e_srcdir = $(datadir)/src
e_src_DATA = netaccess_app.erl netaccess.erl \
		pridrv.erl iisdn.erl \
		netaccess_server.erl netaccess_fsm.erl

docdir = $(datadir)/doc
if EDOC
doc_DATA = ../doc/netaccess.html ../doc/netaccess_fsm.html \
		../doc/iisdn.html ../doc/pridrv.html
endif

if DEBUG
DEBUG_FLAGS = +debug_info
endif

../ebin/%.beam:%.erl
	$(ERLC) -b beam -W -v -o ../ebin -I ../include $(DEBUG_FLAGS) $<

../doc/%.html:%.erl
	$(ERL) -noshell -pa @EDOCPATH@ -run edoc_run file '"$<"' '[{dir,"../doc"}]' -s init stop

../ebin/netaccess.beam:	netaccess.erl ../include/iisdn.hrl ../include/pridrv.hrl
../ebin/netaccess_server.beam:	netaccess_server.erl ../include/iisdn.hrl ../include/pridrv.hrl
../ebin/pridrv.beam:	pridrv.erl ../include/pridrv.hrl
../ebin/iisdn.beam:	iisdn.erl ../include/iisdn.hrl

