## $Id$
##
## Process this file with automake to produce Makefile.in

appdir = $(beamdir)
app_DATA = netaccess.app

beam_DATA = netaccess.beam netaccess_server.beam netaccess_fsm.beam iisdn.beam pridrv.beam

if DEBUG
ERL_DEBUG_FLAGS = +debug_info
endif

%.beam:$(top_srcdir)/src/%.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/include $(ERL_DEBUG_FLAGS) $<

netaccess.beam:	$(top_srcdir)/src/netaccess.erl $(top_builddir)/include/iisdn.hrl $(top_builddir)/include/pridrv.hrl
netaccess_server.beam:	$(top_srcdir)/src/netaccess_server.erl $(top_builddir)/include/iisdn.hrl $(top_builddir)/include/pridrv.hrl
pridrv.beam:	$(top_srcdir)/src/pridrv.erl $(top_builddir)/include/pridrv.hrl
iisdn.beam:	$(top_srcdir)/src/iisdn.erl $(top_builddir)/include/iisdn.hrl

CLEANFILES = $(beam_DATA)

EXTRA_DIST = $(app_DATA)
