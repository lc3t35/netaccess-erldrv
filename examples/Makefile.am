## Process this file with automake to produce Makefile.in

examplesdir = $(datadir)/examples

examples_DATA = lapd_data.erl

CLEANFILES = lapd_data.beam

if DEBUG
	DEBUG_FLAGS = +debug_info
endif

lapd_data.beam:	lapd_data.erl ../include/iisdn.hrl ../include/pridrv.hrl
	$(ERLC) -b beam -W -v -o ../ebin -I ../include $(DEBUG_FLAGS) $<

