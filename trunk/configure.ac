## Process this file with autoconf to produce a configure script.
##
## Boilerplate:  standard boilerplate code
##
AC_INIT(netaccess, 0.1, netaccess-bugs@motivity.ca)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_SRCDIR(src/netaccess_drv.c)
AC_CONFIG_HEADERS(config.h)
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
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(ushort)
AC_CHECK_SIZEOF(uint)
iisdn_includes="#include <stdio.h>
		#include <iisdn.h>"
AC_CHECK_SIZEOF(IISDNu8bit, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDNs8bit, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDNu16bit, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDNs16bit, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDNu32bit, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDNs32bit, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDNp16bit, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDNp32bit, [], [${iisdn_includes}])
SIZEOF_INT=$ac_cv_sizeof_int
SIZEOF_USHORT=$ac_cv_sizeof_ushort
SIZEOF_UINT=$ac_cv_sizeof_uint
SIZEOF_IISDNU8BIT=$ac_cv_sizeof_IISDNu8bit
SIZEOF_IISDNS8BIT=$ac_cv_sizeof_IISDNs8bit
SIZEOF_IISDNU16BIT=$ac_cv_sizeof_IISDNu16bit
SIZEOF_IISDNS16BIT=$ac_cv_sizeof_IISDNs16bit
SIZEOF_IISDNU32BIT=$ac_cv_sizeof_IISDNu32bit
SIZEOF_IISDNS32BIT=$ac_cv_sizeof_IISDNs32bit
SIZEOF_IISDNP16BIT=$ac_cv_sizeof_IISDNp16bit
SIZEOF_IISDNP32BIT=$ac_cv_sizeof_IISDNp32bit
AC_SUBST(SIZEOF_INT)
AC_SUBST(SIZEOF_USHORT)
AC_SUBST(SIZEOF_UINT)
AC_SUBST(SIZEOF_IISDNU8BIT)
AC_SUBST(SIZEOF_IISDNS8BIT)
AC_SUBST(SIZEOF_IISDNU16BIT)
AC_SUBST(SIZEOF_IISDNS16BIT)
AC_SUBST(SIZEOF_IISDNU32BIT)
AC_SUBST(SIZEOF_IISDNS32BIT)
AC_SUBST(SIZEOF_IISDNP16BIT)
AC_SUBST(SIZEOF_IISDNP32BIT)

## use the preprocessor to determine which values these #define's
## get in this environment and export them for building corresponding
## macros in the .hrl files 
AC_MSG_CHECKING([for defined value of IISDN_VERSION])
		_AC_COMPUTE_INT([IISDN_VERSION], [IISDN_VERSION], [${iisdn_includes}],
				[AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDN_VERSION])
		AC_SUBST(IISDN_VERSION)
AC_MSG_CHECKING([for defined value of IISDN_MAX_LINES])
		_AC_COMPUTE_INT([IISDN_MAX_LINES], [IISDN_MAX_LINES], [${iisdn_includes}],
				[AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDN_MAX_LINES])
		AC_SUBST(IISDN_MAX_LINES)
AC_MSG_CHECKING([for defined value of IISDN_NUM_DS1_INTERFACES])
		_AC_COMPUTE_INT([IISDN_NUM_DS1_INTERFACES], [IISDN_NUM_DS1_INTERFACES],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDN_NUM_DS1_INTERFACES])
		AC_SUBST(IISDN_NUM_DS1_INTERFACES)
AC_MSG_CHECKING([for defined value of IISDN_MAX_SPID_LEN])
		_AC_COMPUTE_INT([IISDN_MAX_SPID_LEN], [IISDN_MAX_SPID_LEN],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDN_MAX_SPID_LEN])
		AC_SUBST(IISDN_MAX_SPID_LEN)
AC_MSG_CHECKING([for defined value of IISDN_MAX_DN_LEN])
		_AC_COMPUTE_INT([IISDN_MAX_DN_LEN], [IISDN_MAX_DN_LEN],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDN_MAX_DN_LEN])
		AC_SUBST(IISDN_MAX_DN_LEN)
AC_MSG_CHECKING([for defined value of IISDN_MAX_BOND_CHAN])
		_AC_COMPUTE_INT([IISDN_MAX_BOND_CHAN], [IISDN_MAX_BOND_CHAN],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDN_MAX_BOND_CHAN])
		AC_SUBST(IISDN_MAX_BOND_CHAN)


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
AC_OUTPUT([src/iisdn.hrl src/pridrv.hrl])
AC_OUTPUT([Makefile src/Makefile])
