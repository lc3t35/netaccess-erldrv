## Process this file with automake to produce Makefile.in

data_DATA = netaccess.beam
EXTRA_DIST = netaccess.erl
CLEANFILES = $(beam_DATA)
.erl.beam:
	$(ERLC) -b beam -W -v -DENDIANESS=$(ENDIANESS) \
			-DSIZECHAR=$(SIZECHAR) -DSIZEUCHAR=$(SIZEUCHAR) \
			-DSIZESHORT=$(SIZESHORT) -DSIZEUSHORT=$(SIZEUSHORT) \
			-DSIZEINT=$(SIZEINT) -DSIZEUINT=$(SIZEUINT) \
			-DSIZELONG=$(SIZELONG) -DSIZEULONG=$(SIZEULONG) \
			-DSIZEPOINTER=$(SIZEPOINTER) $<

netaccess.beam:	netaccess.erl

lib_LTLIBRARIES	= netaccess_drv.la
netaccess_drv_la_SOURCES	= netaccess_drv.c
netaccess_drv_la_LDFLAGS	= -module
