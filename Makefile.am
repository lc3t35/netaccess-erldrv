## Process this file with automake to produce Makefile.in

SUBDIRS = c_src include src ebin examples

%.boot:ebin/%.rel
	${ERLC} $<

%.tar.gz:	%.boot
	${ERL} -noshell -pz ebin -run systools make_tar $* -run init stop

CLEANFILES = *.boot *.script

