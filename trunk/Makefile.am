## Process this file with automake to produce Makefile.in

SUBDIRS = c_src include src ebin examples

%.boot:%.rel ebin/netaccess.app
	${ERLC} -I ./ebin $<

%.tar.gz:	%.boot
	${ERL} -noshell -pa ./ebin -run systools make_tar $* -run init stop

CLEANFILES = *.boot *.script

