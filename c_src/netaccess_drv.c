/* --------------------------------------------------------------------- 
 * Copyright Motivity Telecom Inc. 2001-2004
 *
 * All rights reserved. No part of this computer programs(s) may be 
 * used, reproduced, stored in any retrieval system, or transmitted,
 * in any form or by any means, electronic, mechanical, photocopying,
 * recording, or otherwise without prior written permission of  
 * Motivity Telecom Inc.
 * --------------------------------------------------------------------- 
 * 
 * netaccess_drv.c	An Erlang/OTP dynamically loadable driver for 
 *                   the Netaccess HDLC controllers (ISDN/MTP2/etc.)
 *
 * Jan 2001	Vance Shipley <vances@motivity.ca>
 * August 2004 Vance Shipley <vances@motivity.ca>
 *
 * set tabstops=3 (in vi ':set ts=3')
 */

#include <stdio.h>
#include <errno.h>
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#if STDC_HEADERS
# include <stdlib.h>
# include <stddef.h>
#else
# if HAVE_STDLIB_H
#  include <stdlib.h>
# endif
#endif
#if HAVE_STRING_H
# if !STDC_HEADERS && HAVE_MEMORY_H
#  include <memory.h>
# endif
# include <string.h>
#endif
#if HAVE_STRINGS_H
# include <strings.h>
#endif
#if HAVE_INTTYPES_H
# include <inttypes.h>
#else
# if HAVE_STDINT_H
#  include <stdint.h>
# endif
#endif
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_FCNTL_H
#  include <fcntl.h>
#endif

#if HAVE_STROPTS_H
#  include <stropts.h>
#endif

#if HAVE_SYS_UIO_H
#  include <sys/uio.h>
#endif

#if HAVE_ERL_DRIVER_H
#  include "erl_driver.h"
#endif

#if HAVE_EI_H
#  include <ei.h>
#endif

#if HAVE_ENVIRONMENT_H
#  include <environment.h>
#endif
#if HAVE_PRIDRV_H
#  include <pridrv.h>
#else
# if HAVE_WANDRV_H
#  include <wandrv.h>
# endif
#endif
#if HAVE_IISDN_H
# include <iisdn.h>
#endif

#define DEV_PATH "/dev/pri0"
#define DEFAULT_MAXIFRAMESIZE  260
#define DEFAULT_LOWWATER       0
#define DEFAULT_HIGHWATER      4

#define BOOT_BOARD             0
#define ENABLE_MANAGEMENT_CHAN 1
#define RESET_BOARD            2
#define GET_VERSION            3
#define GET_DRIVER_INFO        4
#define SELECT_BOARD           5
#define CANCEL_ASYNC          10
#define FLUSH_QUEUE           11
#define MAXIFRAMESIZE         20
#define LOWWATER              21
#define HIGHWATER             22
#define QSIZE                 23
#define THREAD_L4L3           43
#define THREAD_IFRAME         92

#if defined(DRIVER_DEBUG)
#  define DBG(string) fprintf(stderr, string "\r\n");
#  define DBGARG(string, arg) fprintf(stderr, string "\r\n", arg);
#  define DBGL(string, n) fprintf(stderr, string " %ld\r\n", n);
#  define DBGI(string, n) fprintf(stderr, string " %d\r\n", n);
#else
#  define DBG(string)
#  define DBGARG(string, arg)
#  define DBGL(string, n)
#  define DBGI(string, n)
#endif


static ErlDrvEntry  driver_entry;

/*  This is passed to most of the driver routines, it is our global data  */
typedef struct {
	int fd;                         /* File descriptor             */
	ErlDrvPort port;                /* The port identifier         */
	int maxiframesize;              /* maximum receive IFRAME size */
	int qsize;                      /* Outstanding write requests  */
	int lowwater;                   /* low water mark in queue     */
	int highwater;                  /* high water mark in queue    */
	int busy;                       /* port currently set busy     */
} DriverData;

/*  This data is needed for a thread executing an ioctl request   */
typedef struct {
	struct strioctl *ctlp;          /* streams control data        */
	download_t *bp;                 /* structure to hold boot file */
	ErlDrvBinary *bin;              /* driver binary for result    */
} ThreadIoctlData;

/*  This data is needed for a thread sending an L4L3 SMI message  */
typedef struct {
	struct strbuf strctrl;
} ThreadL4L3Data;

/*  This data is needed for a thread sending an IFRAME message    */
typedef struct {
	int vsize;
	int size;
	SysIOVec *iov;
	ErlDrvBinary **binv;
} ThreadIframeData;

/*  This data is passed around for asynchronous requests  */
typedef struct {
	int command;                    /* ioctl command               */
	int fd;                         /* File descriptor             */
	long ref;                       /* handle to async task        */
	int tresult;                    /* return from async function  */
	int terrno;                     /* errno from async function   */
	union {
		ThreadIoctlData ioctl;       /* iotctl specific data        */
		ThreadL4L3Data l4l3;         /* L4L3 SMI message data       */
		ThreadIframeData iframe;     /* IFRAME message data         */
	} data;
} ThreadData;

extern int erts_async_max_threads;


/**********************************************************************
 **********************************************************************
 *  Define the driver API with Erlang/OTP                             *
 **********************************************************************
 **********************************************************************/

/*  our driver's exported callbacks  */
static ErlDrvData start(ErlDrvPort port, char *command);
static void stop(ErlDrvData handle);
static void finish(void);
static void timeout(ErlDrvData handle);
static void outputv(ErlDrvData handle, ErlIOVec *ev);
static void ready_async(ErlDrvData handle, ErlDrvThreadData t_data);
static void flush(ErlDrvData handle);
static int call(ErlDrvData handle, unsigned int command,
		char *buf, int len, char **rbuf, int rlen, unsigned int *flags);
static void event(ErlDrvData handle, ErlDrvEvent event,
		ErlDrvEventData event_data);


/**********************************************************************
 **********************************************************************
 *  internal functions                                                *
 **********************************************************************
 **********************************************************************/

static void do_l4l3(ThreadData *td);
static void done_l4l3(DriverData *dd, ThreadData *td);
static void free_l4l3(ThreadData *td);
static void do_iframe(ThreadData *td);
static void done_iframe(DriverData *dd, ThreadData *td);
static void free_iframe(ThreadData *td);
static void do_ioctl(ThreadData *td);
static void done_ioctl(DriverData *dd, ThreadData *td);
static void free_ioctl(ThreadData *td);
static int message_to_port(ErlDrvPort port, ErlDrvBinary *ctrl,
		int ctrllen, ErlDrvBinary *data, int datalen);


/**********************************************************************
 **********************************************************************
 *  The exported callbacks which Erlang/OTP uses                      *
 **********************************************************************
 **********************************************************************/


/**********************************************************************
 *  DRIVER_INIT                                                       *
 *                                                                    *
 *  This function is called by Erlang/OTP when this dynamic driver is *
 *  loaded by erl_ddll:load_driver(Path, Name) .  Driver wide         *
 *  initialization takes place here.                                  *
 **********************************************************************/
DRIVER_INIT(netaccess_drv)
{
	DBG("DRIVER_INIT");
	memset(&driver_entry, 0, sizeof(driver_entry));

	/*  called after loading, redundant        */
	driver_entry.init = NULL;

	/*  called when open_port/2 is invoked,
									    return value -1 means failure          */
	driver_entry.start = start;

	/*  called when port is closed, 
									    and when emulator halted               */
	driver_entry.stop = stop;

	/*  called when we have output from
									    erlang to the port                     */
	driver_entry.output = NULL;

	/*  called when we have input from 
									    one of the driver's handles            */
	driver_entry.ready_input = NULL;

	/*  called when output is possible to
									    one of the driver's handles            */
	driver_entry.ready_output = NULL;

	/*  name supplied as command in open_port  */
	driver_entry.driver_name = "netaccess_drv";

	/*  called before unloading the driver     */
	driver_entry.finish = finish;

	/*  handle:  deprecated                    */
	driver_entry.handle = NULL;

	/*  invoked by port_control/3              */
	driver_entry.control = NULL;

	/*  handling of timeout in driver          */
	driver_entry.timeout = timeout;

	/*  called when we have output from
									    erlang to the port                     */
	driver_entry.outputv = outputv;

	/*  ready on the async driver              */
	driver_entry.ready_async = ready_async;

	/*  called when the port is about to be 
									 	 closed, and there is data in the driver
										 queue that needs to be flushed before
										 'stop' can be called                   */
	driver_entry.flush = flush;

	/*  called when port_call/3 is invoked     */
	driver_entry.call = call;

	/*  called when an event selected by  driver_event() has occurred  */
	driver_entry.event = event;

	return &driver_entry;
}


/**********************************************************************
 *  start                                                             *
 *                                                                    *
 *  Erlang/OTP runs this callback in response to an open_port/2.      *
 *  Opens a new file descriptor to the driver.  Each call creates a   *
 *  port attached to a unique file descriptor cloned from the         *
 *  Netaccess STREAMS driver.  Later the user will have to bind it    *
 *  to a specific LAP-D ID.  The caller specifies which board to use  *
 *  (optionally) as an argument to command:                           *
 *       open_port({spawn, 'netaccess_drv /dev/pri0'}, PortSettings)  *
 **********************************************************************/
static ErlDrvData
start(ErlDrvPort port, char *command)
{
	char *s;
	DriverData *dd;
	struct erl_drv_event_data pd;

	DBG("start");
	set_port_control_flags(port, 0); /*  port_control/3 returns a list */
	if(!(dd = driver_alloc(sizeof(DriverData))))
		return ERL_DRV_ERROR_ERRNO;

	/*  pull out the board device name if present  */
	if((s = (char *) strchr(command, ' ')) != NULL) {
		s++;
		/*  open the STREAMS clone device for the Netaccess driver  */
		if ((dd->fd = open(s, O_RDWR)) < 0)
			return ERL_DRV_ERROR_ERRNO;
	} else {    /*  when no device name is present we use default  */
		if ((dd->fd = open(DEV_PATH, O_RDWR)) < 0)
			return ERL_DRV_ERROR_ERRNO;
	}
	pd.events = (POLLIN | POLLRDBAND | POLLPRI);
	driver_event(port, (ErlDrvEvent) dd->fd, (ErlDrvEventData) &pd);
	dd->port = port;	
	dd->maxiframesize = DEFAULT_MAXIFRAMESIZE;
	dd->lowwater = DEFAULT_LOWWATER;
	dd->highwater = DEFAULT_HIGHWATER;
	dd->qsize = 0;
	return((ErlDrvData) dd);
}


/**********************************************************************
 *  stop                                                              *
 *                                                                    *
 *  Erlang/OTP runs this callback in response to a port_close/1.      *
 *  Closes the file descriptor to the driver and frees all associated *
 *  data.  Also called when the driver calls one of the               *
 *  driver_failure_XXX routines.  Note that it is called immediately  *
 *  except in the case where the queue utilities are used in which    *
 *  case it will not be called until the queue is empty.              *
 **********************************************************************/
static void
stop(ErlDrvData handle) 
{
	DriverData *dd = (DriverData *) handle;

	DBG("stop");
	/*  unregister the device handle  */
	driver_event(dd->port, (ErlDrvEvent) dd->fd, 0);

	close(dd->fd);
	driver_free(dd);
}


/**********************************************************************
 *  oputputv                                                          *
 *                                                                    *
 *  Erlang/OTP runs this callback in response to a port_command/2.    *
 *  Supports scatter/gather IO.  When defined in driver_entry this    *
 *  function takes precendence over output().                         *
 *  [Note:  the first element of the IO vector is null so we skip to  *
 *          the second and start from there.  The emulator does this  *
 *          to make it easier for the inet driver to add in a header] *
 *  Receives data from an Erlang process.                             *
 **********************************************************************/
static void
outputv(ErlDrvData handle, ErlIOVec *ev)
{
	DriverData *dd = (DriverData *) handle;
	int i;
	ThreadData *td;

	DBGI("outputv", dd->fd);
	if(!(td = driver_alloc(sizeof(ThreadData)))) {
		driver_failure_posix(dd->port, errno);
		return;
	}
	td->command = THREAD_IFRAME;
	td->fd = dd->fd;
	td->data.iframe.vsize = ev->vsize - 1;
	td->data.iframe.size = ev->size;
	if((td->data.iframe.iov = (SysIOVec *) driver_alloc(ev->vsize *
			sizeof(SysIOVec))) == NULL) {
		driver_free(td);
		driver_failure_posix(dd->port, errno);
		return;
	}
	if((td->data.iframe.binv = (ErlDrvBinary **) driver_alloc(ev->vsize *
			sizeof(ErlDrvBinary *))) == NULL) {
		driver_free(td->data.iframe.iov);
		driver_free(td);
		driver_failure_posix(dd->port, errno);
		return;
	}
	for (i = 1; i < ev->vsize; i++) {
		td->data.iframe.iov[i-1].iov_base = ev->iov[i].iov_base;
		td->data.iframe.iov[i-1].iov_len = ev->iov[i].iov_len;
		td->data.iframe.binv[i-1] = ev->binv[i];
		td->data.iframe.binv[i-1]->refc++;
	}
	td->ref = driver_async(dd->port, NULL, (void *)(void *)do_iframe,
			(void *)td, (void *)(void *)free_iframe);
	DBGL("driver_async", td->ref);
	if (++dd->qsize == dd->highwater) {
		DBG("SET BUSY");
		dd->busy = 1;
		set_busy_port(dd->port, 1);
	}
}
	

/**********************************************************************
 *  finish                                                            *
 *                                                                    *
 *  Erlang/OTP runs this callback when the driver is unloaded with    *
 *  erl_ddll:unload_driver(Name).  Frees all driver data.             *
 **********************************************************************/
static void
finish(void) 
{
	DBG("finish");
}


/**********************************************************************
 *  timeout                                                           *
 *                                                                    *
 *  Erlang/OTP runs this callback when a timeout, specified earlier   *
 *  with driver_set_timer, expires.                                   *
 **********************************************************************/
static void
timeout(ErlDrvData handle)
{
	/*  DriverData *dd = (DriverData *) handle;  */
	DBG("timeout");
}


/**********************************************************************
 *  ready_async                                                       *
 *                                                                    *
 *  Erlang/OTP runs this callback when a previously scheduled async   *
 *  operation, called with driver_async, completes.                   *
 **********************************************************************/
static void
ready_async(ErlDrvData handle, ErlDrvThreadData t_data)
{
	DriverData *dd = (DriverData *) handle;
	ThreadData *td = (ThreadData *) t_data;

	DBGL("ready_async", td->ref);
	if ((--dd->qsize == dd->lowwater) && (dd->busy == 1)) {
		DBGI("UNSET BUSY", dd->fd);
		dd->busy = 0;
		set_busy_port(dd->port, 0);
	}
	switch (td->command) {
		case THREAD_L4L3:
			done_l4l3(dd, td);
			break;
		case THREAD_IFRAME:
			done_iframe(dd, td);
			break;
		case SELECT_BOARD:      
		case BOOT_BOARD:      
		case ENABLE_MANAGEMENT_CHAN:
		case RESET_BOARD:
		case GET_VERSION:
		case GET_DRIVER_INFO:
			done_ioctl(dd, td);
			break;
		default:
			DBGI("unknown thread data type", dd->fd);
	}
}


/**********************************************************************
 *  flush                                                             *
 *                                                                    *
 *  Erlang/OTP runs this callback when there is data in the driver    *
 *  queue and stop is about to be called.                             *
 **********************************************************************/
static void
flush(ErlDrvData handle)
{
	/*  DriverData *dd = (DriverData *) handle;  */

	DBG("flush");
}


/**********************************************************************
 *  call                                                              *
 *                                                                    *
 *  Erlang/OTP runs this callback in response to a port_call/3.       *
 *  Works mostly like port_control but uses the external term format  *
 *  for input and output.  The ei functions are used for decoding the *
 *  the input and ecoding the result.                                 *
 **********************************************************************/
static int
call(ErlDrvData handle, unsigned int command, 
			char *buf, int len, char **rbuf, int rlen, unsigned int *flags)
{
	DriverData *dd = (DriverData *) handle;
	long ref;
	long value;
	ThreadData *td;
	struct strioctl *strioctl;
	int version, index, rindex, type, size;

	DBGI("call", dd->fd);
	index = rindex = size = 0;
   if (ei_decode_version(buf, &index, &version))
      return((int) ERL_DRV_ERROR_GENERAL);

	switch(command) {
		case MAXIFRAMESIZE:
			DBG("MAXIFRAMESIZE");
			if (ei_decode_long(buf, &index, &value))
				return((int) ERL_DRV_ERROR_BADARG);
			if (ei_encode_version(*rbuf, &rindex)
					|| ei_encode_long(*rbuf, &rindex, dd->maxiframesize)) {
				DBG("ei_encode failed");
				return((int) ERL_DRV_ERROR_ERRNO);
			}
			dd->maxiframesize = value;
			return(rindex);
			break;
		case HIGHWATER:
			DBG("HIGHWATER");
			if (ei_decode_long(buf, &index, &value))
				return((int) ERL_DRV_ERROR_BADARG);
			if (ei_encode_version(*rbuf, &rindex)
					|| ei_encode_long(*rbuf, &rindex, dd->highwater)) {
				DBG("ei_encode failed");
				return((int) ERL_DRV_ERROR_ERRNO);
			}
			dd->highwater = value;
			return(rindex);
			break;
		case LOWWATER:
			DBG("LOWWATER");
			if (ei_decode_long(buf, &index, &value))
				return((int) ERL_DRV_ERROR_BADARG);
			if (ei_encode_version(*rbuf, &rindex)
					|| ei_encode_long(*rbuf, &rindex, dd->lowwater)) {
				DBG("ei_encode failed");
				return((int) ERL_DRV_ERROR_ERRNO);
			}
			dd->lowwater = value;
			return(rindex);
			break;
		case QSIZE:
			DBG("QSIZE");
			if (ei_encode_version(*rbuf, &rindex)
					|| ei_encode_long(*rbuf, &rindex, dd->qsize)) {
				DBG("ei_encode failed");
				return((int) ERL_DRV_ERROR_ERRNO);
			}
			return(rindex);
		case CANCEL_ASYNC:
			DBG("CANCEL_ASYNC");
			if (ei_decode_long(buf, &index, &ref))
				return((int) ERL_DRV_ERROR_BADARG);
			if (driver_async_cancel(ref)) {
				if (ei_encode_version(*rbuf, &rindex)
						|| ei_encode_atom(*rbuf, &rindex, "false")) {
					DBG("ei_encode failed");
					return((int) ERL_DRV_ERROR_ERRNO);
				}
			} else {
				DBGL("cancel_async", ref);
				dd->busy = 0;
				if (ei_encode_version(*rbuf, &rindex)
						|| ei_encode_atom(*rbuf, &rindex, "true")) {
					DBG("ei_encode failed");
					return((int) ERL_DRV_ERROR_ERRNO);
				}
			}
			return(rindex);
			break;
	}
	/*  these are potentially blocking tasks so we must be threaded  */
	if (erts_async_max_threads > 0) {
		/*  initialize thread data  */
		if(!(td = (ThreadData *) driver_alloc(sizeof(ThreadData))))
			return((int) ERL_DRV_ERROR_ERRNO);
		memset(td, 0, sizeof(ThreadData));
		td->command = command;
		td->fd = dd->fd;				

		/*  handle a L4L3 SMI control message in a thread  */
		if (command == THREAD_L4L3) {
			if (ei_get_type(buf, &index, &type, &size)
					|| (type != ERL_BINARY_EXT)
					|| !(td->data.l4l3.strctrl.buf = (char *) driver_alloc(size))) {
				driver_free(td);
				return((int) ERL_DRV_ERROR_ERRNO);
			} else {
				if (ei_decode_binary(buf, &index, td->data.l4l3.strctrl.buf,
						(long *) &td->data.l4l3.strctrl.len)) {
					driver_free(td->data.l4l3.strctrl.buf);
					return((int) ERL_DRV_ERROR_BADARG);
				}
				td->ref = driver_async(dd->port, NULL, (void *)(void *)do_l4l3,
						(void *)td, (void *)(void *)free_l4l3);
				DBGL("driver_async", td->ref);
				if (++dd->qsize == dd->highwater) {
					DBGI("SET BUSY", dd->fd);
					dd->busy = 1;
					set_busy_port(dd->port, 1);
				}
				ei_encode_version(*rbuf, &rindex);
				ei_encode_atom(*rbuf, &rindex, "ok");
				return(rindex);
			}
		}

		/*  handle an IOCTL control message in a thread   */
		if(!(strioctl = (struct strioctl *) driver_alloc(sizeof(struct strioctl)))) {
			driver_free(td);
			return((int) ERL_DRV_ERROR_ERRNO);
		}
		memset(strioctl, 0, sizeof(struct strioctl));
		td->data.ioctl.ctlp = strioctl;
		switch(command) {
			case SELECT_BOARD:
				DBG("SELECT_BOARD");
				if (ei_decode_long(buf, &index, &ref))
					return((int) ERL_DRV_ERROR_BADARG);
				strioctl->ic_cmd = PRIDRViocSEL_BOARD;
				strioctl->ic_len = 1;
				strioctl->ic_dp = (char *) driver_alloc(10);  /* board string */
				strioctl->ic_dp[0] = ref;                     /* board num  */
				break;
#ifdef _LP64 /*  passing a pointer in an ioctl requires 64-bit compilation
                 on 64 bit kernels.  if the kernel is 32 bit or if it is
                 64 bit and the Erlang emulator is built 64 bit this works */
			case BOOT_BOARD:      
				DBG("BOOT_BOARD");
				strioctl->ic_cmd = PRIDRViocBOOT;
				strioctl->ic_timout = 60;               /* 60 second timeout   */
				/* we receive a large binary which is the boot image.          */
				/* we must allocate a bootparam structure, initialize it,      */
				/* allocate more space for the binary image and copy the       */
				/* binary image into that space and pass a pointer to the      */
				/* bootparam structure as the data for the ioctl (free later)  */
				if(!(td->data.ioctl.bp = (download_t *) driver_alloc(sizeof(download_t)))) {
					free_ioctl(td);
					return((int) ERL_DRV_ERROR_ERRNO);
				}
				memset(td->data.ioctl.bp, 0, sizeof(download_t));
				strioctl->ic_len = sizeof(download_t);
				strioctl->ic_dp = (char *) td->data.ioctl.bp;
				if (ei_get_type(buf, &index, &type, &size)
						|| (type != ERL_BINARY_EXT)
						|| !(td->data.ioctl.bp->outptr = (char *) driver_alloc(size))) {
					free_ioctl(td);
					return((int) ERL_DRV_ERROR_ERRNO);
				} else {
					if (ei_decode_binary(buf, &index, td->data.ioctl.bp->outptr, 
							&td->data.ioctl.bp->len)) {
						free_ioctl(td);
						return((int) ERL_DRV_ERROR_BADARG);
					}
				}
				break;
#endif	/*  _LP64   */
			case ENABLE_MANAGEMENT_CHAN:
				DBG("ENABLE_MANAGEMENT_CHAN");
				strioctl->ic_cmd = PRIDRViocENA_MGT_CHAN;
				break;
			case RESET_BOARD:
				DBG("RESET_BOARD");
				strioctl->ic_cmd = PRIDRViocRESET_BOARD;
				break;
			case GET_VERSION:
				DBG("GET_VERSION");
				strioctl->ic_cmd = PRIDRViocGET_VERSION;
				strioctl->ic_len = IISDN_VERSION_STRING_LEN;
				if(!(strioctl->ic_dp = (char *) driver_alloc(IISDN_VERSION_STRING_LEN))) {
					free_ioctl(td);
					return((int) ERL_DRV_ERROR_ERRNO);
				}
				break;
			case GET_DRIVER_INFO:
				DBG("GET_DRIVER_INFO");
				strioctl->ic_cmd = PRIDRViocGET_DRIVER_INFO;
				strioctl->ic_len = sizeof(driver_info_t);
				if (!(td->data.ioctl.bin = driver_alloc_binary(sizeof(driver_info_t)))) {
					free_ioctl(td);
					return((int) ERL_DRV_ERROR_ERRNO);
				}
				strioctl->ic_dp = (char *) td->data.ioctl.bin->orig_bytes;
				break;
			default:
				DBG("unknown command");
				free_ioctl(td);
				return((int) ERL_DRV_ERROR_BADARG);
		}
		td->ref = driver_async(dd->port, NULL, (void *)(void *)do_ioctl,
				(void *)td, (void *)(void *)free_ioctl);
		DBGL("driver_async", td->ref);
		if (++dd->qsize == dd->highwater) {
			DBGI("SET BUSY", dd->fd);
			dd->busy = 1;
			set_busy_port(dd->port, 1);
		}
		/*  return the reference  */
		if (ei_encode_version(*rbuf, &rindex)
				|| ei_encode_long(*rbuf, &rindex, td->ref)) {
			DBG("ei_encode failed");
			return((int) ERL_DRV_ERROR_ERRNO);
		}
		return(rindex);
	}
	return((int) ERL_DRV_ERROR_BADARG);
}


/**********************************************************************
 *  event                                                             *
 *                                                                    *
 *  Erlang/OTP runs this callback when an event selected by           *
 *  driver_event() has occurred.                                      *
 **********************************************************************/
static void 
event(ErlDrvData handle, ErlDrvEvent event, ErlDrvEventData event_data)
{     
   DriverData *dd = (DriverData *) handle;
	ErlDrvBinary *ctrlbin, *databin;
	struct strbuf strctrl, strdata;
	int flags = 0;


	DBGI("event", dd->fd);
	if (event_data->revents & POLLERR) {
		DBGI("POLLERR", dd->fd);
		driver_event(dd->port, event, 0);
		driver_failure_posix(dd->port, errno);
	}
	if (event_data->revents & POLLHUP) {
		DBGI("POLLHUP", dd->fd);
		driver_event(dd->port, event, 0);
		driver_failure_posix(dd->port, errno);
	}
	if (event_data->revents & POLLNVAL) {
		DBGI("POLLNVAL", dd->fd);
		driver_event(dd->port, event, 0);
		driver_failure_posix(dd->port, errno);
	}
	if (event_data->revents & (POLLIN|POLLRDNORM|POLLRDBAND|POLLPRI)) {
		/*  construct a streams buffer for the control message  */
		if (!(ctrlbin = driver_alloc_binary(sizeof(L3_to_L4_struct)))) {
			driver_failure_posix(dd->port, errno);
			return;
		}
		strctrl.maxlen = ctrlbin->orig_size;
		strctrl.buf = ctrlbin->orig_bytes;
	
		/*  construct a streams buffer for the data message  */
		if (!(databin = driver_alloc_binary(dd->maxiframesize))) {
			driver_failure_posix(dd->port, errno);
			driver_free_binary(ctrlbin);
			return;
		}
		strdata.maxlen = databin->orig_size;
		strdata.buf = databin->orig_bytes;
	
		/*  read a message from the board  */
		if(getmsg((int) event, &strctrl, &strdata, &flags) < 0) {
			switch(errno) {
				/*  these few errors can be ignored  */
				case EAGAIN:
					DBGI("EAGAIN", dd->fd);
					driver_free_binary(ctrlbin);
					driver_free_binary(databin);
					break;
				case EINTR:
					DBGI("EINTR", dd->fd);
					driver_free_binary(ctrlbin);
					driver_free_binary(databin);
					break;
				/*  other errors cause the port to be closed  */
				default:
					driver_failure_posix(dd->port, errno);
					driver_free_binary(ctrlbin);
					driver_free_binary(databin);
			}
		return;
		}
	
		/*  send the control & data messages to the port owner  */
		message_to_port(dd->port, ctrlbin, ((strctrl.len < 1) ? 0 : strctrl.len),
				databin, ((strdata.len < 1) ? 0 : strdata.len));
	
		driver_free_binary(ctrlbin);
		driver_free_binary(databin);
	}
	
	if (event_data->revents & POLLOUT) {
		DBGI("POLLOUT", dd->fd);
	}
	if (event_data->revents & POLLWRBAND) {
		DBGI("POLLWRBAND", dd->fd);
	}
}



/**********************************************************************
 **********************************************************************
 *  internal functions                                                *
 **********************************************************************
 **********************************************************************/


/**********************************************************************
 *  do_ioctl                                                          *
 *                                                                    *
 *  Called to perform an ioctl on a Netaccess device.  This function  *
 *  is run in a thread by driver_async and Erlang/OTP will call       *
 *  netacess_ready_async when it completes.  We store the return      *
 *  value from the ioctl in the data structure passed in so that we   *
 *  can inspect it later.                                             *
 **********************************************************************/
static void
do_ioctl(ThreadData *td)
{
	DBG("do_iotcl");
	td->tresult = ioctl(td->fd, I_STR, (struct strioctl *) td->data.ioctl.ctlp);
	td->terrno = errno;
}


/**********************************************************************
 *  done_ioctl                                                        *
 *                                                                    *
 *  Called when a previously scheduled do_ioctl has completed.        *
 **********************************************************************/
static void
done_ioctl(DriverData *dd, ThreadData *td)
{
	ErlDrvTermData *ret;

	DBG("done_ioctl");
	if (td->tresult < 0)
		driver_failure_posix(dd->port, td->terrno);
	else
		switch(td->command) {
			case SELECT_BOARD:      
			case BOOT_BOARD:      
			case ENABLE_MANAGEMENT_CHAN:
			case RESET_BOARD:
				/*  {Port, {ref, Ref}, ok}  */
				if (!(ret = driver_alloc(12 * sizeof(ErlDrvTermData)))) {
					driver_failure_posix(dd->port, errno);
					return;
				}
				ret[0] = ERL_DRV_PORT;
				ret[1] = driver_mk_port(dd->port);
				ret[2] = ERL_DRV_ATOM;
				ret[3] = driver_mk_atom("ref");
				ret[4] = ERL_DRV_INT;
				ret[5] = td->ref;
				ret[6] = ERL_DRV_TUPLE;
				ret[7] = 2;
				ret[8] = ERL_DRV_ATOM;
				ret[9] = driver_mk_atom("ok");
				ret[10] = ERL_DRV_TUPLE;
				ret[11] = 3;
				if (driver_output_term(dd->port, ret, 12) < 1)
					DBG("driver_output_term failed");
				driver_free(ret);
				break;
			case GET_VERSION:
				/*  {Port, {ref, Ref}, Version}  */
				if (!(ret = driver_alloc(13 * sizeof(ErlDrvTermData)))) {
					driver_failure_posix(dd->port, errno);
					return;
				}
				ret[0] = ERL_DRV_PORT;
				ret[1] = driver_mk_port(dd->port);
				ret[2] = ERL_DRV_ATOM;
				ret[3] = driver_mk_atom("ref");
				ret[4] = ERL_DRV_INT;
				ret[5] = td->ref;
				ret[6] = ERL_DRV_TUPLE;
				ret[7] = 2;
				ret[8] = ERL_DRV_STRING;
				ret[9] = (ErlDrvTermData) td->data.ioctl.ctlp->ic_dp;
				ret[10] = strlen(td->data.ioctl.ctlp->ic_dp);
				ret[11] = ERL_DRV_TUPLE;
				ret[12] = 3;
				if (driver_output_term(dd->port, ret, 13) < 1)
					DBG("driver_output_term failed");
				driver_free(ret);
				break;
			case GET_DRIVER_INFO:
				/*  {Port, {ref, Ref}, Binary}  */
				if (!(ret = driver_alloc(14 * sizeof(ErlDrvTermData)))) {
					driver_failure_posix(dd->port, errno);
					return;
				}
				ret[0] = ERL_DRV_PORT;
				ret[1] = driver_mk_port(dd->port);
				ret[2] = ERL_DRV_ATOM;
				ret[3] = driver_mk_atom("ref");
				ret[4] = ERL_DRV_INT;
				ret[5] = td->ref;
				ret[6] = ERL_DRV_TUPLE;
				ret[7] = 2;
				ret[8] = ERL_DRV_BINARY;
				ret[9] = (ErlDrvTermData) td->data.ioctl.bin;
				ret[10] = td->data.ioctl.ctlp->ic_len;
				ret[11] = 0;
				ret[12] = ERL_DRV_TUPLE;
				ret[13] = 3;
				if (driver_output_term(dd->port, ret, 14) < 1)
					DBG("driver_output_term failed");
				driver_free(ret);
				break;
		}
	free_ioctl(td);
}


/**********************************************************************
 *  free_ioctl                                                        *
 *                                                                    *
 *  When we specify do_ioctl in a driver_async() call we also specify *
 *  this function to free our thread specific data if it is cancelled *
 *  by the emulator (possibly because we called driver_async_cancel). *
 *                                                                    *
 *  This function should free all of our thread specific data.        *
 **********************************************************************/
static void
free_ioctl(ThreadData *td)
{
	DBGL("free_ioctl", td->ref);
	if (td->data.ioctl.bin != NULL)
		driver_free_binary(td->data.ioctl.bin);
	if (td->data.ioctl.bp != NULL)
		driver_free(td->data.ioctl.bp->outptr);
	driver_free(td->data.ioctl.bp);
	if (td->data.ioctl.ctlp != NULL)
		driver_free(td->data.ioctl.ctlp->ic_dp);
	driver_free(td->data.ioctl.ctlp);
	driver_free(td);
}


/**********************************************************************
 *  do_l4l3                                                           *
 *                                                                    *
 *  Called to send an L4L3 SMI control message.  This function is run *
 *  in a thread by driver_async.                                      *
 **********************************************************************/
static void
do_l4l3(ThreadData *td)
{
	DBG("do_l4l3");
	/*  send a control message to the board with high priority  */
	td->tresult = putmsg(td->fd, &td->data.l4l3.strctrl, NULL, RS_HIPRI);
	td->terrno = errno;
	/* TODO:  set thread priority  */
}


/**********************************************************************
 *  done_l4l3                                                         *
 *                                                                    *
 *  Called when a previously scheduled do_l4l3 has completed.         *
 **********************************************************************/
static void
done_l4l3(DriverData *dd, ThreadData *td)
{
	DBGL("done_l4l3", td->ref);
	if (td->tresult < 0)
		driver_failure_posix(dd->port, td->terrno);
 	free_l4l3(td);
}


/**********************************************************************
 *  free_l4l3                                                         *
 *                                                                    *
 *  When we specify do_l4l3 in a driver_async() call we also specify  *
 *  this function to free our thread specific data if it is cancelled *
 *  by the emulator (possibly because we called driver_async_cancel). *
 *                                                                    *
 *  This function should free all of our thread specific data.        *
 **********************************************************************/
static void
free_l4l3(ThreadData *td)
{
	DBGL("free_l4l3", td->ref);
	driver_free(td->data.l4l3.strctrl.buf);
	driver_free(td);
}


/**********************************************************************
 *  do_iframe                                                         *
 *                                                                    *
 *  Called to send an IFRAME data message.  This function is run in a *
 *  thread by driver_async.                                           *
 **********************************************************************/
static void
do_iframe(ThreadData *td)
{
	DBG("do_iframe");
	td->tresult = writev(td->fd, td->data.iframe.iov, td->data.iframe.vsize);
	td->terrno = errno;
}


/**********************************************************************
 *  done_iframe                                                       *
 *                                                                    *
 *  Called when a previously scheduled do_iframe has completed.       *
 **********************************************************************/
static void
done_iframe(DriverData *dd, ThreadData *td)
{
	DBGL("done_iframe", td->ref);
	if (td->tresult < 0)
{
		driver_failure_posix(dd->port, td->terrno);
}
	free_iframe(td);
}


/**********************************************************************
 *  free_iframe                                                       *
 *                                                                    *
 *  When we specify do_iframe in a driver_async() call we also        *
 *  specify this function to free our thread specific data if it is   *
 *  cancelled by the emulator (possibly because we called             *
 *  driver_async_cancel).                                             *
 *                                                                    *
 *  This function should free all of our thread specific data.        *
 **********************************************************************/
static void
free_iframe(ThreadData *td)
{
	int i;

	DBGL("free_iframe", td->ref);
	driver_free(td->data.iframe.iov);
	for(i = 0; i < td->data.iframe.vsize; i++)
		if (td->data.iframe.binv[i] != NULL)
			driver_free_binary(td->data.iframe.binv[i]);
	driver_free(td->data.iframe.binv);
	driver_free(td);
}


/*  output control and data messages {Port, {'L3L4m', <<Control>>,<<Data>>}}  */
int
message_to_port(ErlDrvPort port, ErlDrvBinary *ctrl, int ctrllen,
		ErlDrvBinary *data, int datalen)
{
	ErlDrvTermData msg[16];

	msg[0] = ERL_DRV_PORT;
	msg[1] = driver_mk_port(port);
	msg[2] = ERL_DRV_ATOM;
	msg[3] = driver_mk_atom("L3L4m");
	msg[4] = ERL_DRV_BINARY;
	msg[5] = (ErlDrvTermData) ctrl;
	msg[6] = ctrllen;
	msg[7] = 0;
	msg[8] = ERL_DRV_BINARY;
	msg[9] = (ErlDrvTermData) data;
	msg[10] = datalen;
	msg[11] = 0;
	msg[12] = ERL_DRV_TUPLE;
	msg[13] = 3;
	msg[14] = ERL_DRV_TUPLE;
	msg[15] = 2;

	return driver_output_term(port, msg, 16);
}

