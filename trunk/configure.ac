## Process this file with autoconf to produce a configure script.
##
## Boilerplate:  standard boilerplate code
##
AC_INIT(netaccess, 1.0, netaccess-bugs@motivity.ca)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_SRCDIR(c_src/netaccess_drv.c)
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
datadir=$ERL_TOP/lib/$PACKAGE_NAME-$PACKAGE_VERSION
libdir=$ERL_TOP/lib/$PACKAGE_NAME-$PACKAGE_VERSION/priv/lib

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

AC_ARG_WITH(edoc,
	[  --with-edoc=PATH        load edoc from PATH],
	[ case "${withval}" in
		no) edoc=false ;;
		yes) ;;
		*) EDOCPATH="${EDOCPATH} ${withval}" ;;
	esac ])
AM_CONDITIONAL(EDOC, test x"${edoc}" != "xfalse")

AC_ARG_WITH(syntax_tools,
	[  --with-syntax_tools=PATH  load syntax_tools from PATH],
	[ case "${withval}" in
		no) ;;
		yes) ;;
		*) EDOCPATH="${EDOCPATH} ${withval}" ;;
	esac ])

AC_ARG_WITH(xmerl,
	[  --with-xmerl=PATH       load xmerl from PATH],
	[ case "${withval}" in
		no) ;;
		yes) ;;
		*) EDOCPATH="${EDOCPATH} ${withval}" ;;
	esac ])
AC_SUBST(EDOCPATH)


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
AC_CONFIG_FILES([Makefile src/Makefile c_src/Makefile include/Makefile examples/Makefile examples/lapd_data/Makefile ebin/Makefile])
AC_OUTPUT
