## Process this file with automake to produce Makefile.in

beam_DATA = ebin/netaccess.beam ebin/netaccess_server.beam \
		ebin/pridrv.beam ebin/iisdn.beam 

EXTRA_DIST = src/netaccess.erl src/netaccess_server.erl \
		src/pridrv.erl \
		include/pridrv.hrl include/iisdn.erl

CLEANFILES = $(beam_DATA) include/pridrv.hrl include/iisdn.erl

if DEBUG
.erl.beam:
	$(ERLC) -b beam -W -v -o ../ebin -I ../include +debug_info $<
else
.erl.beam:
	$(ERLC) -b beam -W -v -o ../ebin -I ../include $<
fi

ebin/netaccess.beam:	src/netaccess.erl include/iisdn.hrl include/pridrv.hrl
ebin/netaccess_server.beam:	src/netaccess_server.erl \
		include/iisdn.hrl include/pridrv.hrl
ebin/pridrv.beam:	src/pridrv.erl include/pridrv.hrl
ebin/iisdn.beam:	src/iisdn.erl include/iisdn.hrl


lib_LTLIBRARIES = netaccess_drv.la
netaccess_drv_la_SOURCES = netaccess_drv.c
netaccess_drv_la_LDFLAGS = -module
