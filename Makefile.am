## Process this file with automake to produce Makefile.in

SUBDIRS = priv/lib c_src include src examples

%.boot:ebin/%.rel
	${ERLC} -pa ./ebin $<

%.tar.gz:	%.boot
	${ERL} -noshell -pa ./ebin -run systools make_tar $* -run init stop

CLEANFILES = *.boot *.script

