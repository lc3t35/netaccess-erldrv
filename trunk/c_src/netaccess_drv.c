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

/*  TODO:  make this a runtime option  */
#define MAXIFRAMESZ 260     /*  maximum size of a received IFRAME  */
#define LOWWATER (4 * MAXIFRAMESZ)
#define HIGHWATER (8 * MAXIFRAMESZ)

#define SET_NONBLOCKING(fd)     fcntl((fd), F_SETFL, \
                                      fcntl((fd), F_GETFL, 0) | O_NONBLOCK)
#define QKEY NULL
#define CANCEL_ASYNC 10
#define FLUSH_QUEUE  11

#define BOOT_BOARD 0
#define ENABLE_MANAGEMENT_CHAN 1
#define RESET_BOARD  2
#define GET_VERSION 3
#define GET_DRIVER_INFO 4
#define SELECT_BOARD 5

#if defined(DEBUG)
#  define DBG(string) fprintf(stderr, string "\r\n");
#  define DBGARG(string, arg) fprintf(stderr, string "\r\n", arg);
#else
#  define DBG(string)
#  define DBGARG(string, arg)
#endif


/**********************************************************************
 **********************************************************************
 *  internal functions                                                *
 **********************************************************************
 **********************************************************************/

static void do_ioctl(void *t_data);
static void free_tdata(void *t_data);
static int message_to_board(int fd, SysIOVec *iov);
static int message_to_port(ErlDrvPort port, ErlDrvBinary *ctrl,
		int ctrllen, ErlDrvBinary *data, int datalen);


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


static ErlDrvEntry  driver_entry;

/*  This is passed to most of the driver routines, it is our global data  */
typedef struct {
	int fd;                         /* File descriptor */
	ErlDrvPort port;                /* The port identifier */
	int low;                        /* low water mark */
	int high;                       /* high water mark */
} DriverData;

/*  This data is needed for a thread executing an ioctl request  */
typedef struct {
	struct strioctl *ctlp;          /* streams control data        */
	download_t *bp;                 /* structure to hold boot file */
	ErlDrvBinary *bin;              /* driver binary for result    */
} ThreadIoctlData;

/*  This data is needed for a thread sending an L4L3 SMI message  */
typedef struct {
	SysIOVec ctrlbin;
} ThreadL4L3Data;

/*  This data is needed for a thread sending an IFRAME message    */
typedef struct {
	SysIOVec databin;
} ThreadIframeData;

/*  This data is passed around for asynchronous requests  */
typedef struct {
#define TD_IOCTL  0
#define TD_L4L3   1
#define TD_IFRAME 2
	int type;                       /* ioctl, l4l3 or iframe       */
	int fd;                         /* File descriptor             */
	int command;                    /* ioctl command               */
	long ref;                       /* handle to async task        */
	int result;                     /* return from async function  */
	int terrno;                     /* errno from async function   */
	union {
		ThreadIoctlData ioctl;       /* iotctl specific data        */
		ThreadL4L3Data l3l4;         /* L4L3 SMI message data       */
		ThreadIframeData iframe;     /* IFRAME message data         */
	} data;
} ThreadData;

extern int erts_async_max_threads;

/**********************************************************************
 **********************************************************************
 *  Netaccess declarations                                            *
 **********************************************************************
 **********************************************************************/


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

	if((dd = driver_alloc(sizeof(DriverData))) == NULL)
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

	SET_NONBLOCKING(dd->fd);
	pd.events = (POLLIN | POLLRDBAND | POLLPRI);
	driver_event(port, (ErlDrvEvent) dd->fd, (ErlDrvEventData) &pd);

	dd->port = port;	
	dd->low = LOWWATER;       /*  queue low water mark   */
	dd->high = HIGHWATER;     /*  queue high water mark  */

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
	int sz;
	struct erl_drv_event_data pd;

	DBG("outputv");
	/*  if there's a queue just add to it  */
	if((sz = driver_sizeq(dd->port)) > 0) {
			driver_enqv(dd->port, ev, 0);
			if((sz + 1) >= dd->high) {          /*  queue full, throttle  */
				DBG("queue high water mark");
				set_busy_port(dd->port, 1);
			} return;
	}

	/*  send the message to the board  */	
	if(message_to_board(dd->fd, &ev->iov[1]) < 0) {
		DBG("message_to_board failed");
		switch(errno) {
			case EINTR:
			case ENOSR:
			case EAGAIN:
				driver_enqv(dd->port, ev, 0);
				pd.events = POLLOUT;
				driver_event(dd->port, (ErlDrvEvent) dd->fd, &pd);
				break;
			default:
				driver_event(dd->port, (ErlDrvEvent) dd->fd, 0);
				driver_failure_posix(dd->port, errno);
		}
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
	ErlDrvTermData *ret;

	DBG("ready_async");

	if (td->result < 0) {
		/*  {Port, {ref, Ref}, {error, Reason}}  */
		if (!(ret = driver_alloc(16 * sizeof(ErlDrvTermData)))) {
			DBG("driver_alloc failed");
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
		ret[9] = driver_mk_atom("error");
		ret[10] = ERL_DRV_ATOM;
		ret[11] = driver_mk_atom(erl_errno_id(td->terrno));
		ret[12] = ERL_DRV_TUPLE;
		ret[13] = 2;
		ret[14] = ERL_DRV_TUPLE;
		ret[15] = 3;
		if (driver_output_term(dd->port, ret, 16) < 1)
			DBG("driver_output_term failed");
		driver_free(ret);
		return;
	} 
		
	else
		switch(td->command) {
			case SELECT_BOARD:      
			case BOOT_BOARD:      
			case ENABLE_MANAGEMENT_CHAN:
			case RESET_BOARD:
				/*  {Port, {ref, Ref}, ok}  */
				if (!(ret = driver_alloc(12 * sizeof(ErlDrvTermData)))) {
					DBG("driver_alloc failed");
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
				/*  {Port, {ref, Ref}, {ok, Version}}  */
				if (!(ret = driver_alloc(17 * sizeof(ErlDrvTermData)))) {
					DBG("driver_alloc failed");
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
				ret[10] = ERL_DRV_STRING;
				ret[11] = (ErlDrvTermData) td->data.ioctl.ctlp->ic_dp;
				ret[12] = strlen(td->data.ioctl.ctlp->ic_dp);
				ret[13] = ERL_DRV_TUPLE;
				ret[14] = 2;
				ret[15] = ERL_DRV_TUPLE;
				ret[16] = 3;
				if (driver_output_term(dd->port, ret, 17) < 1)
					DBG("driver_output_term failed");
				driver_free(ret);
				break;
			case GET_DRIVER_INFO:
				/*  {Port, {ref, Ref}, {ok, Binary}}  */
				if (!(ret = driver_alloc(18 * sizeof(ErlDrvTermData)))) {
					DBG("driver_alloc failed");
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
				ret[10] = ERL_DRV_BINARY;
				ret[11] = (ErlDrvTermData) td->data.ioctl.bin;
				ret[12] = td->data.ioctl.ctlp->ic_len;
				ret[13] = 0;
				ret[14] = ERL_DRV_TUPLE;
				ret[15] = 2;
				ret[16] = ERL_DRV_TUPLE;
				ret[17] = 3;
				if (driver_output_term(dd->port, ret, 18) < 1)
					DBG("driver_output_term failed");
				driver_free(ret);
				break;
		}
	free_tdata(td);
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
	int qsize;
	ThreadData *td;
	struct strioctl *cntl_ptr;
	int version, index, rindex, size;

	DBG("call");
	index = rindex = size = 0;
   if (ei_decode_version(buf, &index, &version))
      return((int) ERL_DRV_ERROR_GENERAL);

	switch(command) {
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
				if (ei_encode_version(*rbuf, &rindex)
						|| ei_encode_atom(*rbuf, &rindex, "true")) {
					DBG("ei_encode failed");
					return((int) ERL_DRV_ERROR_ERRNO);
				}
			}
			return(rindex);
			break;
		case FLUSH_QUEUE:
			DBG("FLUSH_QUEUE");
    		qsize = driver_sizeq(dd->port);
			driver_deq(dd->port, qsize);        
			if (ei_encode_version(*rbuf, &rindex)
					|| ei_encode_atom(*rbuf, &rindex, "true")) {
				DBG("ei_encode failed");
				return((int) ERL_DRV_ERROR_ERRNO);
			} else
				return(rindex);
	}
	/*  these are potentially blocking tasks so we must be threaded  */
	if (erts_async_max_threads > 0) {
		/*  initialize thread data  */
		td = (ThreadData *) driver_alloc(sizeof(ThreadData));
		memset(td, 0, sizeof(ThreadData));
		td->fd = dd->fd;				
		td->command = command;
		cntl_ptr = (struct strioctl *) driver_alloc(sizeof(struct strioctl));
		memset(cntl_ptr, 0, sizeof(struct strioctl));
		td->data.ioctl.ctlp = cntl_ptr;

		switch(command) {
			case SELECT_BOARD:
				DBG("SELECT_BOARD");
				if (ei_decode_long(buf, &index, &ref))
					return((int) ERL_DRV_ERROR_BADARG);
				cntl_ptr->ic_cmd = PRIDRViocSEL_BOARD;
				cntl_ptr->ic_len = 1;                
				cntl_ptr->ic_dp = (char *) driver_alloc(10);  /* board string */  
				cntl_ptr->ic_dp[0] = ref;                     /* board num  */
				break;
#ifdef _LP64 /*  passing a pointer in an ioctl requires 64-bit compilation
                 on 64 bit kernels.  if the kernel is 32 bit or if it is
                 64 bit and the Erlang emulator is built 64 bit this works */
			case BOOT_BOARD:      
				DBG("BOOT_BOARD");
				cntl_ptr->ic_cmd = PRIDRViocBOOT;
				cntl_ptr->ic_timout = 60;               /* 60 second timeout   */
				/* we receive a large binary which is the boot image.          */
				/* we must allocate a bootparam structure, initialize it,      */
				/* allocate more space for the binary image and copy the       */
				/* binary image into that space and pass a pointer to the      */
				/* bootparam structure as the data for the ioctl (free later)  */
				td->data.ioctl.bp = (download_t *) driver_alloc(sizeof(download_t));
				memset(td->data.ioctl.bp, 0, sizeof(download_t));
				cntl_ptr->ic_len = sizeof(download_t);
				cntl_ptr->ic_dp = (char *) td->data.ioctl.bp;
				if (ei_get_type(buf, &index, &type, &size)
						|| (type != ERL_BINARY_EXT)
						|| !(td->data.ioctl.bp->outptr = (char *) driver_alloc(count))) {
					free_tdata(td);
					return((int) ERL_DRV_ERROR_ERRNO);
				} else {
					if (ei_decode_binary(buf, &index, td->data.ioctl.bp->outptr, 
							&td->data.ioctl.bp->len))
						return((int) ERL_DRV_ERROR_BADARG);
				}
				break;
#endif	/*  _LP64   */
			case ENABLE_MANAGEMENT_CHAN:
				DBG("ENABLE_MANAGEMENT_CHAN");
				cntl_ptr->ic_cmd = PRIDRViocENA_MGT_CHAN;
				break;
			case RESET_BOARD:
				DBG("RESET_BOARD");
				cntl_ptr->ic_cmd = PRIDRViocRESET_BOARD;
				break;
			case GET_VERSION:
				DBG("GET_VERSION");
				cntl_ptr->ic_cmd = PRIDRViocGET_VERSION;
				cntl_ptr->ic_len = IISDN_VERSION_STRING_LEN;
				cntl_ptr->ic_dp = (char *) driver_alloc(IISDN_VERSION_STRING_LEN);
				break;
			case GET_DRIVER_INFO:
				DBG("GET_DRIVER_INFO");
				cntl_ptr->ic_cmd = PRIDRViocGET_DRIVER_INFO;
				cntl_ptr->ic_len = sizeof(driver_info_t);
				if (!(td->data.ioctl.bin = driver_alloc_binary(sizeof(driver_info_t)))) {
					free_tdata(td);
					return((int) ERL_DRV_ERROR_ERRNO);
				}
				cntl_ptr->ic_dp = (char *) td->data.ioctl.bin->orig_bytes;
				break;
			default:
				DBG("unknown command");
				free_tdata(td);
				return((int) ERL_DRV_ERROR_BADARG);
		}
		td->ref = driver_async(dd->port, QKEY,
				do_ioctl, td, free_tdata);
		/*  return the reference  */
		if (ei_encode_version(*rbuf, &rindex)
				|| ei_encode_tuple_header(*rbuf, &rindex, 2)
				|| ei_encode_atom(*rbuf, &rindex, "ok")
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
	int vsize, qsize;
	SysIOVec *iov;


	DBG("event");
	if (event_data->revents & (POLLIN|POLLRDNORM|POLLRDBAND|POLLPRI)) {
		/*  construct a streams buffer for the control message  */
		if (!(ctrlbin = driver_alloc_binary(sizeof(L3_to_L4_struct)))) {
			driver_failure_posix(dd->port, errno);
			return;
		}
		strctrl.maxlen = ctrlbin->orig_size;
		strctrl.buf = ctrlbin->orig_bytes;
	
		/*  construct a streams buffer for the data message  */
		if (!(databin = driver_alloc_binary(MAXIFRAMESZ))) {
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
				case EINTR:
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
		DBG("POLLOUT");
		while((iov = driver_peekq(dd->port, &vsize)) != NULL) {
			qsize = driver_sizeq(dd->port);
			/*  send the next message to the board  */	
			if(message_to_board(dd->fd, iov) < 0) {
				switch(errno) {
					case EINTR:
					case EAGAIN:
						driver_event(dd->port, event, event_data);
						return;
					default:
						driver_deq(dd->port, qsize);  /*  dump the whole queue  */
						driver_failure_posix(dd->port, errno);
						return;
				}
			}
			driver_deq(dd->port, iov[0].iov_len); /* dequeue this sent message */
			if(qsize <= dd->low) {              /*  queue emptying, unthrottle  */
				DBG("queue low water mark");
				set_busy_port(dd->port, 0);
			}
		}
		event_data->events = (event_data->events & ~POLLOUT);
		driver_event(dd->port, event, event_data);
	}

	if (event_data->revents & POLLWRBAND) {
		DBG("POLLWRBAND");
	}
	if (event_data->revents & POLLERR) {
		DBG("POLLERR");
fprintf(stderr, "POLLERR encountered, errno = %d, revents = 0x%x\n", errno, event_data->revents);
		driver_event(dd->port, event, 0);
		driver_failure_posix(dd->port, errno);
	}
	if (event_data->revents & POLLHUP) {
		DBG("POLLHUP");
		driver_event(dd->port, event, 0);
		driver_failure_posix(dd->port, errno);
	}
	if (event_data->revents & POLLNVAL) {
		DBG("POLLNVAL");
		driver_event(dd->port, event, 0);
		driver_failure_posix(dd->port, errno);
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
do_ioctl(void *t_data)
{
	ThreadData *td = (ThreadData *) t_data;

	DBG("do_iotcl");
	td->result = ioctl(td->fd, I_STR, (struct strioctl *) td->data.ioctl.ctlp);
	if (td->result < 0) 
		td->terrno = errno;
}


/**********************************************************************
 *  free_tdata                                                        *
 *                                                                    *
 *  Erlang/OTP runs this callback when a previously scheduled async   *
 *  operation, called with driver_async, must be canceled.  This      *
 *  should free the data buffer passed to driver_async.               *
 **********************************************************************/
static void
free_tdata(void *t_data)
{
	ThreadData *td = (ThreadData *) t_data;

	DBG("free_tdata");
	if (td->data.ioctl.bin != NULL)
		driver_free_binary(td->data.ioctl.bin);
	if (td->data.ioctl.bp != NULL) {
		if (td->data.ioctl.bp->outptr != NULL)
			driver_free(td->data.ioctl.bp->outptr);
		driver_free(td->data.ioctl.bp);
	}
	if (td->data.ioctl.ctlp != NULL) {
		if (td->data.ioctl.ctlp->ic_dp != NULL)
			driver_free(td->data.ioctl.ctlp->ic_dp);
		driver_free(td->data.ioctl.ctlp);
	}
	if (t_data != NULL)
		driver_free(t_data);
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


/*  send an SMI message to the board  */
int
message_to_board(int fd, SysIOVec *iov)
{
	struct strbuf buf;

	/*  the first byte is the type; control or data  */
	if(iov->iov_base[0] == 0) {
		buf.len = (iov->iov_len - 1);
		buf.buf = &iov->iov_base[1];
		/*  send a control message to the board with high priority  */
		return(putmsg(fd, &buf, NULL, RS_HIPRI));
	} else {
		buf.len = (iov->iov_len - 1);
		buf.buf = &iov->iov_base[1];
		/*  send a data message to the board  */
		return(putmsg(fd, NULL, &buf, 0));
	}
}
