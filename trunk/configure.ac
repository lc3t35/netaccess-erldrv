## Process this file with autoconf to produce a configure script.
##
## Boilerplate:  standard boilerplate code
##
AC_INIT(netaccess, 0.1, netaccessbugs@motivity.ca)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_SRCDIR(src/netaccess_drv.c)
AM_CONFIG_HEADER(config.h)
AM_INIT_AUTOMAKE


##
## Options:  control options to configure, support code for the options
##
CFLAGS="${CFLAGS} -Wall"
AC_DISABLE_STATIC
AC_LIBTOOL_DLOPEN
AC_PROG_LIBTOOL 
if test -z "$ERL_TOP" ; then
	AC_MSG_ERROR([you must set ERL_TOP to the root of the Erlang installation.])
fi
datadir=$ERL_TOP/lib/$PACKAGE_NAME-$PACKAGE_VERSION/ebin
libdir=$ERL_TOP/lib/$PACKAGE_NAME-$PACKAGE_VERSION/priv/lib

AC_ARG_ENABLE(debug,
	[  --enable-debug    Turn on debugging],
	[case "${enableval}" in
		yes)
			CPPFLAGS="${CPPFLAGS} -DDEBUG"
			;;
		no)
			;;
		*) 
			AC_MSG_ERROR(bad value ${enableval} for --enable-debug)
			;;
	esac],
	[debug=false])

AC_ARG_ENABLE(threads,
	[  --enable-threads   build for a threaded emulator],
	[ case "${enableval}" in
		no)  threaded=no ;;
		yes) threaded=yes ;;
		*)  AC_MSG_ERROR(bad value ${enableval} for --enable-threads) ;;
	esac ], threaded=no)

AC_ARG_ENABLE(select,
	[  --enable-select    use the select() system call],
	[ case "${enableval}" in
		no)  use_select=no ;;
		yes) use_select=yes ;;
		*)  AC_MSG_ERROR(bad value ${enableval} for --enable-select) ;;
	esac ], use_select=no)
if test "x$use_select" = "xyes" ; then
	CPPFLAGS="${CPPFLAGS} -DUSE_SELECT"
fi

##
## Programs:  check for programs needed by the configure process,
##            the build process, or by one of the programs being built
##
AC_PROG_CC
AC_PROG_INSTALL
if test -z "${ERLC}" ; then
	AC_PATH_PROG(ERLC, erlc, [], [${ERL_TOP}/bin:${ERL_TOP}/bootstrap/bin:$PATH])
fi
if test -z "${ERLC}" ; then
	AC_MSG_ERROR([cannot find Erlang compiler in path or ${ERLC}.])
fi
AC_ARG_VAR(ERL_TOP, [Erlang top level directory])
AC_ARG_VAR(ERLC, [Erlang compiler command])


##
## Libraries:  check for libraries
##
# find the ei (erl_interface )libraries
AC_MSG_CHECKING([for erl_interface])
erl_interface_path=`ls $ERL_TOP/lib | grep "erl_interface*" | tail -1`
test ! -d ${erl_interface_path} || AC_MSG_ERROR([cannot locate erl_interface.])
AC_MSG_RESULT(${erl_interface_path})
LDFLAGS="${LDFLAGS} -L${ERL_TOP}/lib/${erl_interface_path}/lib"



##
## Headers:  check for header files
##
CPPFLAGS="${CPPFLAGS} -I${ERL_TOP}/usr/include"
CPPFLAGS="${CPPFLAGS} -I${ERL_TOP}/lib/${erl_interface_path}/include"
AC_CHECK_HEADERS([fcntl.h unistd.h])
AC_CHECK_HEADERS([stropts.h])
AC_CHECK_HEADERS([erl_driver.h])
AC_CHECK_HEADERS([ei.h])

# set up for a threaded emulator
AC_CHECK_HEADERS([thread.h] [pthread.h])
case ${threaded} in
	yes)
		CPPFLAGS="${CPPFLAGS} -DUSE_THREADS -D_REENTRANT"
		case ${host_os} in
			solaris*)
				CPPFLAGS="${CPPFLAGS} -D_POSIX_PTHREAD_SEMANTICS"
				;;	
			*)
				AC_MSG_ERROR([Don't know how to enable threads on ${host_os}])
				;;
		esac
		;;
	no)
		;;
	*)
		AC_MSG_ERROR([Unknown value ${threaded} for --enable-threads])
		;;
esac

## find the netaccess include files
AC_ARG_VAR(NETACC_TOP, [Netaccess driver package top level directory])
if test -n "$NETACC_TOP" ; then
	netaccdirs="$NETACC_TOP/include/sys $NETACC_TOP/include"
else
	netaccdirs="/opt/NETACCpri/include/sys /usr/local/wandrv/include"
fi
path_valid=no
for netaccdir in $netaccdirs
do
	AC_CHECK_HEADER([$netaccdir/pridrv.h],
			[AC_DEFINE(HAVE_PRIDRV_H, 1,
			[Define to 1 if you have the <pridrv.h> header file.])
			path_valid=yes])
	AC_CHECK_HEADER([$netaccdir/wandrv.h],
			[AC_DEFINE(HAVE_WANDRV_H, 1,
			[Define to 1 if you have the <wandrv.h> header file.])
			path_valid=yes])
	AC_CHECK_HEADER([$netaccdir/naii.h],
			[AC_DEFINE(HAVE_NAII_H, 1,
			[Define to 1 if you have the <naii.h> header file.])
			path_valid=yes])
	AC_CHECK_HEADER([$netaccdir/iisdn.h],
			[AC_DEFINE(HAVE_IISDN_H, 1,
			[Define to 1 if you have the <iisdn.h> header file.])
			path_valid=yes])
	AC_CHECK_HEADER([$netaccdir/environment.h],
			[AC_DEFINE(HAVE_ENVIRONMENT_H, 1,
			[Define to 1 if you have the <environment.h> header file.])
			path_valid=yes])
	if test $path_valid = yes ; then
		CPPFLAGS="$CPPFLAGS -I$netaccdir"
		break
	fi
done
	

##
## Typedefs & Structures:  check for typedefs, structures, 
##                         and compiler characteristics.
##
AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(unsigned char)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(unsigned short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(unsigned int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(unsigned long)
AC_CHECK_SIZEOF(char *)
SIZECHAR=$ac_cv_sizeof_char
SIZEUCHAR=$ac_cv_sizeof_unsigned_char
SIZESHORT=$ac_cv_sizeof_short
SIZEUSHORT=$ac_cv_sizeof_unsigned_short
SIZEINT=$ac_cv_sizeof_int
SIZEUINT=$ac_cv_sizeof_unsigned_int
SIZELONG=$ac_cv_sizeof_long
SIZEULONG=$ac_cv_sizeof_unsigned_long
SIZEPOINTER=$ac_cv_sizeof_char_p
AC_SUBST(SIZECHAR)
AC_SUBST(SIZEUCHAR)
AC_SUBST(SIZESHORT)
AC_SUBST(SIZEUSHORT)
AC_SUBST(SIZEINT)
AC_SUBST(SIZEUINT)
AC_SUBST(SIZELONG)
AC_SUBST(SIZEULONG)
AC_SUBST(SIZEPOINTER)


##
## Functions:  check for library functions
##
AC_HEADER_STDC
AC_CHECK_FUNCS([memset strchr])
if test "x$threaded" = xyes ; then
	AC_CHECK_LIB(thread, thr_create)
fi
AC_CHECK_LIB(ei, ei_x_new)


##
## Output:  create output files
##
AC_OUTPUT([Makefile src/Makefile])
