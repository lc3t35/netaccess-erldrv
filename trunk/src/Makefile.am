## Process this file with automake to produce Makefile.in

data_DATA = netaccess.beam netaccess_server.beam \
		pridrv.beam iisdn.beam \
		q931_test.beam tsi_test.beam abis.beam
EXTRA_DIST = netaccess.erl netaccess_server.erl \
		pridrv.erl pridrv.hrl \
		iisdn.erl iisdn.hrl \
		q931_test.erl tsi_test.erl abis,erl
CLEANFILES = $(beam_DATA)
.erl.beam:
	$(ERLC) -b beam -W -v $<

netaccess.beam:	netaccess.erl iisdn.hrl pridrv.hrl
netaccess_server.beam:	netaccess_server.erl 
pridrv.beam:	pridrv.erl pridrv.hrl
iisdn.beam:	iisdn.erl iisdn.hrl
q931_test.beam:	q931_test.erl iisdn.hrl
test_test.beam:	test_test.erl iisdn.hrl
abis.beam:	abis.erl iisdn.hrl

lib_LTLIBRARIES	= netaccess_drv.la
netaccess_drv_la_SOURCES	= netaccess_drv.c
netaccess_drv_la_LDFLAGS	= -module
