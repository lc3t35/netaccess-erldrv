## $Id$
##
## Process this file with autoconf to produce a configure script.
##
## Boilerplate:  standard boilerplate code
##
AC_INIT(netaccess, 1.1, netaccess-bugs@motivity.ca)
AC_COPYRIGHT([Copyright (c) 2001-2005 Motivity Telecom Inc])
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_SRCDIR(c_src/netaccess_drv.c)
AC_CONFIG_HEADERS(config.h)
AM_INIT_AUTOMAKE


##
## Options:  control options to configure, support code for the options
##
AC_CANONICAL_HOST
case "${host}" in
	*-solaris*)
		case `isainfo -b` in
			64)
				AC_DEFINE(KERNEL_64, [], "64-bit kernel")
				;;
			*)
				;;
		esac	
		;;
esac

CFLAGS="${CFLAGS} -Wall"
AC_DISABLE_STATIC
AC_LIBTOOL_DLOPEN
AC_PROG_LIBTOOL 
if test -z "$ERL_TOP" ; then
	AC_MSG_ERROR([you must set ERL_TOP to the root of the Erlang installation.])
fi
beamdir=$ERL_TOP/lib/$PACKAGE_NAME-$PACKAGE_VERSION/ebin
libdir=$ERL_TOP/lib/$PACKAGE_NAME-$PACKAGE_VERSION/priv/lib
edocdir=$ERL_TOP/lib/$PACKAGE_NAME-$PACKAGE_VERSION/doc/html
eincludedir=$ERL_TOP/lib/$PACKAGE_NAME-$PACKAGE_VERSION/include
esrcdir=$ERL_TOP/lib/$PACKAGE_NAME-$PACKAGE_VERSION/src
csrcdir=$ERL_TOP/lib/$PACKAGE_NAME-$PACKAGE_VERSION/c_src
examplesdir=$ERL_TOP/lib/$PACKAGE_NAME-$PACKAGE_VERSION/examples
AC_SUBST(beamdir)
AC_SUBST(edocdir)
AC_SUBST(eincludedir)
AC_SUBST(esrcdir)
AC_SUBST(csrcdir)
AC_SUBST(examplesdir)

AC_ARG_ENABLE(debug,
	[  --enable-debug          Turn on debugging],
	[case "${enableval}" in
		yes)
			debug=true
			;;
		no)
			debug=false
			;;
		*) 
			AC_MSG_ERROR(bad value ${enableval} for --enable-debug)
			;;
	esac], [debug=false] )
AM_CONDITIONAL(DEBUG, test x"$debug" = "xtrue")

AC_ARG_ENABLE(threads,
	[  --enable-threads        build for a threaded emulator],
	[ case "${enableval}" in
		no)  threaded=no ;;
		yes) threaded=yes ;;
		*)  AC_MSG_ERROR(bad value ${enableval} for --enable-threads) ;;
	esac ], threaded=no )

AC_ARG_ENABLE(select,
	[  --enable-select         use the select() system call],
	[ case "${enableval}" in
		no)  use_select=no ;;
		yes) use_select=yes ;;
		*)  AC_MSG_ERROR(bad value ${enableval} for --enable-select) ;;
	esac ], use_select=no )
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
if test -z "${ERL}" ; then
	AC_PATH_PROG(ERL, erl, [], [${ERL_TOP}/bin:${ERL_TOP}/bootstrap/bin:$PATH])
fi
if test -z "${ERL}" ; then
	AC_MSG_ERROR([cannot find Erlang emuluator in path or ${ERL}.])
fi
AC_ARG_VAR(ERL_TOP, [Erlang top level directory])
AC_ARG_VAR(ERLC, [Erlang compiler command])
AC_ARG_VAR(ERL, [Erlang emulator command])
if test -z "${ERLCFLAGS}" ; then
	ERLCFLAGS="-W -v"
fi
AC_SUBST(ERLCFLAGS)


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
AC_CHECK_HEADERS([sys/uio.h])
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
AC_CHECK_SIZEOF(IISDN_LINE_DATA, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_L2_LAP_PARAMS, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_L2_UDPIP_PARAMS, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_L2_TCPIP_PARAMS, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_L2_DPNSS_PARAMS, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_L2_SS7_PARAMS, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_L2_V110_PARAMS, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_L2_LAP_CONSTS, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_L2_IP_CONSTS, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_L2_DPNSS_CONSTS, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_L2_SS7_CONSTS, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_Q931_CNFG, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_BONDING_DATA, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_X25_CONFIG, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_PM_CONFIG, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_RELAY_CONFIG, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_DPNSSCC_CONFIG, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_DASSCC_CONFIG, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_Q933A_CONFIG, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_Q933A_PVC_STATUS, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_ALARM_STATUS, [], [${iisdn_includes}])
AC_CHECK_SIZEOF(IISDN_PP, [], [${iisdn_includes}])
SIZEOF_int=$ac_cv_sizeof_int
SIZEOF_ushort=$ac_cv_sizeof_ushort
SIZEOF_uint=$ac_cv_sizeof_uint
SIZEOF_IISDNu8bit=$ac_cv_sizeof_IISDNu8bit
SIZEOF_IISDNs8bit=$ac_cv_sizeof_IISDNs8bit
SIZEOF_IISDNu16bit=$ac_cv_sizeof_IISDNu16bit
SIZEOF_IISDNs16bit=$ac_cv_sizeof_IISDNs16bit
SIZEOF_IISDNu32bit=$ac_cv_sizeof_IISDNu32bit
SIZEOF_IISDNs32bit=$ac_cv_sizeof_IISDNs32bit
SIZEOF_IISDNp16bit=$ac_cv_sizeof_IISDNp16bit
SIZEOF_IISDNp32bit=$ac_cv_sizeof_IISDNp32bit
SIZEOF_IISDN_LINE_DATA=$ac_cv_sizeof_IISDN_LINE_DATA
SIZEOF_IISDN_L2_LAP_PARAMS=$ac_cv_sizeof_IISDN_L2_LAP_PARAMS
SIZEOF_IISDN_L2_UDPIP_PARAMS=$ac_cv_sizeof_IISDN_L2_UDPIP_PARAMS
SIZEOF_IISDN_L2_TCPIP_PARAMS=$ac_cv_sizeof_IISDN_L2_TCPIP_PARAMS
SIZEOF_IISDN_L2_DPNSS_PARAMS=$ac_cv_sizeof_IISDN_L2_DPNSS_PARAMS
SIZEOF_IISDN_L2_SS7_PARAMS=$ac_cv_sizeof_IISDN_L2_SS7_PARAMS
SIZEOF_IISDN_L2_V110_PARAMS=$ac_cv_sizeof_IISDN_L2_V110_PARAMS
SIZEOF_IISDN_L2_LAP_CONSTS=$ac_cv_sizeof_IISDN_L2_LAP_CONSTS
SIZEOF_IISDN_L2_IP_CONSTS=$ac_cv_sizeof_IISDN_L2_IP_CONSTS
SIZEOF_IISDN_L2_DPNSS_CONSTS=$ac_cv_sizeof_IISDN_L2_DPNSS_CONSTS
SIZEOF_IISDN_L2_SS7_CONSTS=$ac_cv_sizeof_IISDN_L2_SS7_CONSTS
SIZEOF_IISDN_Q931_CNFG=$ac_cv_sizeof_IISDN_Q931_CNFG
SIZEOF_IISDN_BONDING_DATA=$ac_cv_sizeof_IISDN_BONDING_DATA
SIZEOF_IISDN_X25_CONFIG=$ac_cv_sizeof_IISDN_X25_CONFIG
SIZEOF_IISDN_PM_CONFIG=$ac_cv_sizeof_IISDN_PM_CONFIG
SIZEOF_IISDN_RELAY_CONFIG=$ac_cv_sizeof_IISDN_RELAY_CONFIG
SIZEOF_IISDN_DPNSSCC_CONFIG=$ac_cv_sizeof_IISDN_DPNSSCC_CONFIG
SIZEOF_IISDN_DASSCC_CONFIG=$ac_cv_sizeof_IISDN_DASSCC_CONFIG
SIZEOF_IISDN_Q933A_CONFIG=$ac_cv_sizeof_IISDN_Q933A_CONFIG
SIZEOF_IISDN_Q933A_PVC_STATUS=$ac_cv_sizeof_IISDN_Q933A_PVC_STATUS
SIZEOF_IISDN_ALARM_STATUS=$ac_cv_sizeof_IISDN_ALARM_STATUS
SIZEOF_IISDN_PP=$ac_cv_sizeof_IISDN_PP
AC_SUBST(SIZEOF_int)
AC_SUBST(SIZEOF_ushort)
AC_SUBST(SIZEOF_uint)
AC_SUBST(SIZEOF_IISDNu8bit)
AC_SUBST(SIZEOF_IISDNs8bit)
AC_SUBST(SIZEOF_IISDNu16bit)
AC_SUBST(SIZEOF_IISDNs16bit)
AC_SUBST(SIZEOF_IISDNu32bit)
AC_SUBST(SIZEOF_IISDNs32bit)
AC_SUBST(SIZEOF_IISDNp16bit)
AC_SUBST(SIZEOF_IISDNp32bit)
AC_SUBST(SIZEOF_IISDN_LINE_DATA)
AC_SUBST(SIZEOF_IISDN_L2_LAP_PARAMS)
AC_SUBST(SIZEOF_IISDN_L2_UDPIP_PARAMS)
AC_SUBST(SIZEOF_IISDN_L2_TCPIP_PARAMS)
AC_SUBST(SIZEOF_IISDN_L2_DPNSS_PARAMS)
AC_SUBST(SIZEOF_IISDN_L2_SS7_PARAMS)
AC_SUBST(SIZEOF_IISDN_L2_V110_PARAMS)
AC_SUBST(SIZEOF_IISDN_L2_LAP_CONSTS)
AC_SUBST(SIZEOF_IISDN_L2_IP_CONSTS)
AC_SUBST(SIZEOF_IISDN_L2_DPNSS_CONSTS)
AC_SUBST(SIZEOF_IISDN_L2_SS7_CONSTS)
AC_SUBST(SIZEOF_IISDN_Q931_CNFG)
AC_SUBST(SIZEOF_IISDN_BONDING_DATA)
AC_SUBST(SIZEOF_IISDN_X25_CONFIG)
AC_SUBST(SIZEOF_IISDN_PM_CONFIG)
AC_SUBST(SIZEOF_IISDN_RELAY_CONFIG)
AC_SUBST(SIZEOF_IISDN_DPNSSCC_CONFIG)
AC_SUBST(SIZEOF_IISDN_DASSCC_CONFIG)
AC_SUBST(SIZEOF_IISDN_Q933A_CONFIG)
AC_SUBST(SIZEOF_IISDN_Q933A_PVC_STATUS)
AC_SUBST(SIZEOF_IISDN_ALARM_STATUS)
AC_SUBST(SIZEOF_IISDN_PP)

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
AC_MSG_CHECKING([for defined value of IISDN_MAX_VC_PER_CHAN])
		_AC_COMPUTE_INT([IISDN_MAX_VC_PER_CHAN], [IISDN_MAX_VC_PER_CHAN],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDN_MAX_VC_PER_CHAN])
		AC_SUBST(IISDN_MAX_VC_PER_CHAN)
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
AC_MSG_CHECKING([for defined value of IISDN_RELAY_MAX_MATCHES])
		_AC_COMPUTE_INT([IISDN_RELAY_MAX_MATCHES], [IISDN_RELAY_MAX_MATCHES],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDN_RELAY_MAX_MATCHES])
		AC_SUBST(IISDN_RELAY_MAX_MATCHES)
AC_MSG_CHECKING([for defined value of IISDN_RELAY_MAX_INSERT])
		_AC_COMPUTE_INT([IISDN_RELAY_MAX_INSERT], [IISDN_RELAY_MAX_INSERT],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDN_RELAY_MAX_INSERT])
		AC_SUBST(IISDN_RELAY_MAX_INSERT)
AC_MSG_CHECKING([for defined value of IISDN_RELAYdestDCHAN])
		_AC_COMPUTE_INT([IISDN_RELAYdestDCHAN], [IISDN_RELAYdestDCHAN],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDN_RELAYdestDCHAN])
		AC_SUBST(IISDN_RELAYdestDCHAN)
AC_MSG_CHECKING([for defined value of IISDN_RELAYdestLID])
		_AC_COMPUTE_INT([IISDN_RELAYdestLID], [IISDN_RELAYdestLID],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDN_RELAYdestLID])
		AC_SUBST(IISDN_RELAYdestLID)
AC_MSG_CHECKING([for defined value of IISDN_RELAYdestLID_DEFERRED_PRIORITY])
		_AC_COMPUTE_INT([IISDN_RELAYdestLID_DEFERRED_PRIORITY], [IISDN_RELAYdestLID_DEFERRED_PRIORITY],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDN_RELAYdestLID_DEFERRED_PRIORITY])
		AC_SUBST(IISDN_RELAYdestLID_DEFERRED_PRIORITY)
AC_MSG_CHECKING([for defined value of IISDN_RELAYdestDROP])
		_AC_COMPUTE_INT([IISDN_RELAYdestDROP], [IISDN_RELAYdestDROP],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDN_RELAYdestDROP])
		AC_SUBST(IISDN_RELAYdestDROP)
AC_MSG_CHECKING([for defined value of IISDN_RELAYdestLIST])
		_AC_COMPUTE_INT([IISDN_RELAYdestLIST], [IISDN_RELAYdestLIST],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDN_RELAYdestLIST])
		AC_SUBST(IISDN_RELAYdestLIST)
AC_MSG_CHECKING([for defined value of IISDN_RELAYdestBLOCK])
		_AC_COMPUTE_INT([IISDN_RELAYdestBLOCK], [IISDN_RELAYdestBLOCK],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDN_RELAYdestBLOCK])
		AC_SUBST(IISDN_RELAYdestBLOCK)
AC_MSG_CHECKING([for defined value of IISDN_RELAYdestDCHAN_OF_LID])
		_AC_COMPUTE_INT([IISDN_RELAYdestDCHAN_OF_LID], [IISDN_RELAYdestDCHAN_OF_LID],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDN_RELAYdestDCHAN_OF_LID])
		AC_SUBST(IISDN_RELAYdestDCHAN_OF_LID)
AC_MSG_CHECKING([for defined value of IISDNdevDEFAULT])
		_AC_COMPUTE_INT([IISDNdevDEFAULT], [IISDNdevDEFAULT],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevDEFAULT])
		AC_SUBST(IISDNdevDEFAULT)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_0])
		_AC_COMPUTE_INT([IISDNdevHDLC_0], [IISDNdevHDLC_0],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_0])
		AC_SUBST(IISDNdevHDLC_0)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_1])
		_AC_COMPUTE_INT([IISDNdevHDLC_1], [IISDNdevHDLC_1],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_1])
		AC_SUBST(IISDNdevHDLC_1)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_2])
		_AC_COMPUTE_INT([IISDNdevHDLC_2], [IISDNdevHDLC_2],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_2])
		AC_SUBST(IISDNdevHDLC_2)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_3])
		_AC_COMPUTE_INT([IISDNdevHDLC_3], [IISDNdevHDLC_3],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_3])
		AC_SUBST(IISDNdevHDLC_3)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_4])
		_AC_COMPUTE_INT([IISDNdevHDLC_4], [IISDNdevHDLC_4],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_4])
		AC_SUBST(IISDNdevHDLC_4)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_5])
		_AC_COMPUTE_INT([IISDNdevHDLC_5], [IISDNdevHDLC_5],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_5])
		AC_SUBST(IISDNdevHDLC_5)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_6])
		_AC_COMPUTE_INT([IISDNdevHDLC_6], [IISDNdevHDLC_6],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_6])
		AC_SUBST(IISDNdevHDLC_6)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_7])
		_AC_COMPUTE_INT([IISDNdevHDLC_7], [IISDNdevHDLC_7],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_7])
		AC_SUBST(IISDNdevHDLC_7)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_8])
		_AC_COMPUTE_INT([IISDNdevHDLC_8], [IISDNdevHDLC_8],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_8])
		AC_SUBST(IISDNdevHDLC_8)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_9])
		_AC_COMPUTE_INT([IISDNdevHDLC_9], [IISDNdevHDLC_9],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_9])
		AC_SUBST(IISDNdevHDLC_9)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_10])
		_AC_COMPUTE_INT([IISDNdevHDLC_10], [IISDNdevHDLC_10],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_10])
		AC_SUBST(IISDNdevHDLC_10)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_11])
		_AC_COMPUTE_INT([IISDNdevHDLC_11], [IISDNdevHDLC_11],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_11])
		AC_SUBST(IISDNdevHDLC_11)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_12])
		_AC_COMPUTE_INT([IISDNdevHDLC_12], [IISDNdevHDLC_12],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_12])
		AC_SUBST(IISDNdevHDLC_12)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_13])
		_AC_COMPUTE_INT([IISDNdevHDLC_13], [IISDNdevHDLC_13],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_13])
		AC_SUBST(IISDNdevHDLC_13)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_14])
		_AC_COMPUTE_INT([IISDNdevHDLC_14], [IISDNdevHDLC_14],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_14])
		AC_SUBST(IISDNdevHDLC_14)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_15])
		_AC_COMPUTE_INT([IISDNdevHDLC_15], [IISDNdevHDLC_15],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_15])
		AC_SUBST(IISDNdevHDLC_15)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_16])
		_AC_COMPUTE_INT([IISDNdevHDLC_16], [IISDNdevHDLC_16],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_16])
		AC_SUBST(IISDNdevHDLC_16)
AC_MSG_CHECKING([for defined value of IISDNdevMODEM_0])
		_AC_COMPUTE_INT([IISDNdevMODEM_0], [IISDNdevMODEM_0],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevMODEM_0])
		AC_SUBST(IISDNdevMODEM_0)
AC_MSG_CHECKING([for defined value of IISDNdevMODEM_1])
		_AC_COMPUTE_INT([IISDNdevMODEM_1], [IISDNdevMODEM_1],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevMODEM_1])
		AC_SUBST(IISDNdevMODEM_1)
AC_MSG_CHECKING([for defined value of IISDNdevSYNC_0])
		_AC_COMPUTE_INT([IISDNdevSYNC_0], [IISDNdevSYNC_0],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevSYNC_0])
		AC_SUBST(IISDNdevSYNC_0)
AC_MSG_CHECKING([for defined value of IISDNdevFDL])
		_AC_COMPUTE_INT([IISDNdevFDL], [IISDNdevFDL],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevFDL])
		AC_SUBST(IISDNdevFDL)
AC_MSG_CHECKING([for defined value of IISDNdevHDLC_LINE])
		_AC_COMPUTE_INT([IISDNdevHDLC_LINE], [IISDNdevHDLC_LINE],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevHDLC_LINE])
		AC_SUBST(IISDNdevHDLC_LINE)
AC_MSG_CHECKING([for defined value of IISDNdevSIGNALING])
		_AC_COMPUTE_INT([IISDNdevSIGNALING], [IISDNdevSIGNALING],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevSIGNALING])
		AC_SUBST(IISDNdevSIGNALING)
AC_MSG_CHECKING([for defined value of IISDNdevETHERNET])
		_AC_COMPUTE_INT([IISDNdevETHERNET], [IISDNdevETHERNET],
				[${iisdn_includes}], [AC_MSG_ERROR([undefined.])])
		AC_MSG_RESULT([$IISDNdevETHERNET])
		AC_SUBST(IISDNdevETHERNET)


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

AC_CONFIG_FILES([include/iisdn.hrl include/pridrv.hrl])
AC_CONFIG_FILES([Makefile src/Makefile c_src/Makefile include/Makefile
		doc/Makefile doc/html/Makefile examples/Makefile examples/lapd_data/Makefile ebin/Makefile])
AC_OUTPUT
