## Process this file with automake to produce Makefile.in

data_DATA = netaccess.beam naii.beam
EXTRA_DIST = netaccess.erl naii.erl naii.hrl pridrv.hrl
CLEANFILES = $(beam_DATA)
.erl.beam:
	$(ERLC) -b beam -W -v $<

netaccess.beam:	netaccess.erl naii.hrl pridrv.hrl
naii.beam:	naii.erl naii.hrl

lib_LTLIBRARIES	= netaccess_drv.la
netaccess_drv_la_SOURCES	= netaccess_drv.c
netaccess_drv_la_LDFLAGS	= -module
