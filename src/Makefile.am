## Process this file with automake to produce Makefile.in

data_DATA = netaccess.beam pridrv.beam iisdn.beam isdn_test.beam
EXTRA_DIST = netaccess.erl pridrv.erl iisdn.erl iisdn.hrl pridrv.hrl \
		isdn_test.beam
CLEANFILES = $(beam_DATA)
.erl.beam:
	$(ERLC) -b beam -W -v $<

netaccess.beam:	netaccess.erl iisdn.hrl pridrv.hrl
pridrv.beam:	pridrv.erl pridrv.hrl
iisdn.beam:	iisdn.erl iisdn.hrl
isdn_test.beam:	iisdn.hrl pridrv.hrl

lib_LTLIBRARIES	= netaccess_drv.la
netaccess_drv_la_SOURCES	= netaccess_drv.c
netaccess_drv_la_LDFLAGS	= -module
