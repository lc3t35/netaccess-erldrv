## Process this file with automake to produce Makefile.in

c_srcdir = $(datadir)/c_src

c_src_DATA = netaccess_drv.c

if DEBUG
AM_CFLAGS = -DDRIVER_DEBUG
endif

lib_LTLIBRARIES = netaccess_drv.la
netaccess_drv_la_SOURCES = netaccess_drv.c
netaccess_drv_la_LDFLAGS = -module

