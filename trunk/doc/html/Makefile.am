## $Id$
#
## Process this file with automake to produce Makefile.in

edoc_DATA = netaccess.html netaccess_fsm.html iisdn.html pridrv.html

%.html:$(top_srcdir)/src/%.erl
	$(ERL) -noshell -run edoc_run file '"$<"' '[{dir,"."}]' -s init stop

CLEANFILES = $(edoc_DATA)

EXTRA_DIST = $(edoc_DATA)

