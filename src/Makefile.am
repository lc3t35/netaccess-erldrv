## Process this file with automake to produce Makefile.in

beamdir = $(datadir)/ebin
beam_DATA = $(beamdir)/netaccess.beam $(beamdir)/pridrv.beam \
		$(beamdir)/iisdn.beam $(beamdir)/netaccess_server.beam \
		$(beamdir)/netaccess_fsm.beam

e_srcdir = $(datadir)/src
e_src_DATA = netaccess.erl pridrv.erl iisdn.erl  \
		netaccess_server.erl netaccess_fsm.erl

docdir = $(datadir)/doc/html
if EDOC
doc_DATA = $(docdir)/netaccess.html $(docdir)/netaccess_fsm.html \
		$(docdir)/iisdn.html $(docdir)/pridrv.html
endif

CLEANFILES = $(beam_DATA) $(doc_DATA)

if DEBUG
DEBUG_FLAGS = +debug_info
endif

$(beamdir)/%.beam:%.erl
	$(ERLC) -b beam -W -v -o ../ebin -I ../include $(DEBUG_FLAGS) $<

$(docdir)/%.html:%.erl
	$(ERL) -noshell -pa @EDOCPATH@ -run edoc_run file '"$<"' '[{dir,"$(docdir)"}]' -s init stop

../ebin/netaccess.beam:	netaccess.erl ../include/iisdn.hrl \
		../include/pridrv.hrl
../ebin/netaccess_server.beam:	netaccess_server.erl \
		../include/iisdn.hrl ../include/pridrv.hrl
../ebin/pridrv.beam:	pridrv.erl ../include/pridrv.hrl
../ebin/iisdn.beam:	iisdn.erl ../include/iisdn.hrl

