/* --------------------------------------------------------------------- 
 * Copyright Motivity Telecom Inc. 2001, 2002 
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
#if HAVE_NAII_H
#  include <naii.h>
#else
# if HAVE_IISDN_H
#  include <iisdn.h>
# endif
#endif

#define DEV_PATH "/dev/pri0"

#define BUFSIZE 256             /* <= rx_bufsize */

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




/**********************************************************************
 **********************************************************************
 *  internal functions                                                *
 **********************************************************************
 **********************************************************************/

static void do_ioctl(void *t_data);
static void free_tdata(void *t_data);

/**********************************************************************
 **********************************************************************
 *  Define the driver API with Erlang/OTP                             *
 **********************************************************************
 **********************************************************************/

/*  our driver's exported callbacks  */
static ErlDrvData netaccess_start(ErlDrvPort port, char *command);
static void netaccess_stop(ErlDrvData handle);
static void netaccess_output(ErlDrvData handle, char *buf, int len);
static void netaccess_ready_input(ErlDrvData handle, ErlDrvEvent event);
static void netaccess_ready_output(ErlDrvData handle, ErlDrvEvent event);
static void netaccess_finish(void);
static int netaccess_control(ErlDrvData handle, unsigned int command,
				char *buf, int len, char **rbuf, int rlen);
static void netaccess_timeout(ErlDrvData handle);
static void netaccess_outputv(ErlDrvData handle, ErlIOVec *ev);
static void netacess_ready_async(ErlDrvData handle, ErlDrvThreadData t_data);
static void netaccess_flush(ErlDrvData handle);
static int netaccess_call(ErlDrvData handle, unsigned int command,
			char *buf, int len, char **rbuf, int rlen, unsigned int *flags);


ErlDrvEntry  netaccess_driver_entry;


/*  This is passed to most of the driver routines, it is our global data  */
typedef struct dd {
	int fd;                         /* File descriptor */
	ErlDrvPort port;                /* The port identifier */
	int low;                        /* low water mark */
	int high;                       /* high water mark */
} DriverData;

/*  This data is passed around for asynchronous requests  */
typedef struct td {
	int fd;                         /* File descriptor             */
	int command;                    /* ioctl command               */
	long ref;                       /* handle to async task        */
	struct strioctl *ctlp;          /* streams control data        */ 
	download_t *bp;                 /* structure to hold boot file */
	int result;                     /* return from async function  */
	int errno;                      /* errno from async function   */
	ErlDrvBinary *bin;              /* driver binary for result    */
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
fprintf(stderr, "DRIVER_INIT\n\r");
	memset(&netaccess_driver_entry, 0, sizeof(netaccess_driver_entry));

	/*  called after loading, redundant        */
	netaccess_driver_entry.init = NULL;

	/*  called when open_port/2 is invoked,
									    return value -1 means failure          */
	netaccess_driver_entry.start = netaccess_start;

	/*  called when port is closed, 
									    and when emulator halted               */
	netaccess_driver_entry.stop = netaccess_stop;

	/*  called when we have output from
									    erlang to the port                     */
	netaccess_driver_entry.output = netaccess_output;

	/*  called when we have input from 
									    one of the driver's handles            */
	netaccess_driver_entry.ready_input = netaccess_ready_input;

	/*  called when output is possible to
									    one of the driver's handles            */
	netaccess_driver_entry.ready_output = netaccess_ready_output;

	/*  name supplied as command in open_port  */
	netaccess_driver_entry.driver_name = "netaccess_drv";

	/*  called before unloading the driver     */
	netaccess_driver_entry.finish = netaccess_finish;

	/*  handle:  deprecated                    */
	netaccess_driver_entry.handle = NULL;

	/*  invoked by port_control/3              */
	netaccess_driver_entry.control = netaccess_control;

	/*  handling of timeout in driver          */
	netaccess_driver_entry.timeout = netaccess_timeout;

	/*  called when we have output from
									    erlang to the port                     */
	netaccess_driver_entry.outputv = netaccess_outputv;

	/*  ready on the async driver              */
	netaccess_driver_entry.ready_async = netacess_ready_async;

	/*  called when the port is about to be 
									 	 closed, and there is data in the driver
										 queue that needs to be flushed before
										 'stop' can be called                   */
	netaccess_driver_entry.flush = netaccess_flush;

	/*  called when port_call/3 is invoked     */
	netaccess_driver_entry.call = netaccess_call;

	/*  called when an event selected by  driver_event() has occurred  */
	netaccess_driver_entry.event = NULL;

	return &netaccess_driver_entry;
}


/**********************************************************************
 *  netaccess_start                                                   *
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
netaccess_start(ErlDrvPort port, char *command)
{
	char *s;
	DriverData *dd;

fprintf(stderr, "netaccess_start\n\r");
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
	driver_select(port, (ErlDrvEvent) dd->fd, DO_READ, 1);

	dd->port = port;	

	return((ErlDrvData) dd);
}


/**********************************************************************
 *  netaccess_stop                                                    *
 *                                                                    *
 *  Erlang/OTP runs this callback in response to a port_close/1.      *
 *  Closes the file descriptor to the driver and frees all associated *
 *  data.  Also called when the driver calls one of the               *
 *  driver_failure_XXX routines.  Note that it is called immediately  *
 *  except in the case where the queue utilities are used in which    *
 *  case it will not be called until the queue is empty.              *
 **********************************************************************/
static void
netaccess_stop(ErlDrvData handle) 
{
	DriverData *dd = (DriverData *) handle;

fprintf(stderr, "netaccess_stop\n\r");
	/*  unregister the device handle  */
	driver_select(dd->port, (ErlDrvEvent) dd->fd, DO_READ, 0);
	driver_select(dd->port, (ErlDrvEvent) dd->fd, DO_WRITE, 0);

	close(dd->fd);
	driver_free(dd);
}


/**********************************************************************
 *  netaccess_output                                                  *
 *                                                                    *
 *  Erlang/OTP runs this callback in response to a port_command/2 if  *
 *  (driver_entry.outputv == NULL).  Accepts simple byte buffer.  If  *
 *  netaccess_outputv is used this will never be used.                 *
 *  Receives data from an Erlang process.                             *
 **********************************************************************/
static void
netaccess_output(ErlDrvData handle, char *buff, int bufflen)
{
	DriverData *dd = (DriverData *) handle;
fprintf(stderr, "netaccess_output\n\r");

}


/**********************************************************************
 *  netaccess_oputputv                                                *
 *                                                                    *
 *  Erlang/OTP runs this callback in response to a port_command/2.    *
 *  Supports scatter/gather IO.  When defined in driver_entry this    *
 *  function takes precendence over output (netaccess_output).        *
 *  [Note:  the first element of the IO vector is null so we skip to  *
 *          the second and start from there.  The emulator does this  *
 *          to make it easier for the inet driver to add in a header] *
 *  Receives data from an Erlang process.                             *
 **********************************************************************/
static void
netaccess_outputv(ErlDrvData handle, ErlIOVec *ev)
{
	DriverData *dd = (DriverData *) handle;
	int sz;

	/*  if there's a queue just add to it  */
	if((sz = driver_sizeq(dd->port)) > 0) {
			driver_enqv(dd->port, ev, 0);
			if((sz + 1) >= dd->high)           /*  queue full, throttle  */
				set_busy_port(dd->port, 1);
			return;
	}

	/*  send the message to the board  */	
	if(message_to_board(dd->fd, &ev->iov[1]) < 0) {
		switch(errno) {
			case EINTR:
			case ENOSR:
			case EAGAIN:
				driver_enqv(dd->port, ev, 0);
				driver_select(dd->port, (ErlDrvEvent) dd->fd, DO_WRITE, 1);
				break;
			default:
				driver_failure_posix(dd->port, errno);
		}
	}
}
	

/**********************************************************************
 *  netaccess_control                                                 *
 *                                                                    *
 *  Erlang/OTP runs this callback in response to a port_control/3.    *
 *  Receives a command & data from Erlang and sends reply data.       *
 *  The second argument is the command passed as argument 2 in        *
 *  port_control/3.  The third argument is a pointer to the binary    *
 *  which will be passed to the driver ioctl function as is.  The     *
 *  fourth argument is a pointer to a buffer in which we may store a  *
 *  response.  The response will be the reference to the async task   *
 *  which is launched to service the ioctl request and can be used to *
 *  later cancel the request.  The sixth argument is the length       *
 *  of the supplied buffer.  If the supplied buffer is not large      *
 *  enough to hold the response we may allocate a larger one and      *
 *  replace the pointer value with the address of the new buffer.     *
 *  This function should return the number of bytes supplied in the   *
 *  response buffer or -1 to indicate the command was not recognized. *
 **********************************************************************/
static int
netaccess_control(ErlDrvData handle, unsigned int command, 
			char* buf, int count, char** res, int res_size)
{
	DriverData *dd = (DriverData *) handle;
	int qsize;
	int int_result;        /*  use to hold int returns  */ 
	ThreadData *td;
	struct strioctl *cntl_ptr;

fprintf(stderr, "netaccess_control\n\r");
	switch(command) {
		case CANCEL_ASYNC:
			int_result = driver_async_cancel(atol(*buf));
			memcpy(*res, &int_result, sizeof(int_result));
			return sizeof(int_result);
		case FLUSH_QUEUE:
    		qsize = driver_sizeq(dd->port);
			int_result = driver_deq(dd->port, qsize);        
			memcpy(*res, &int_result, sizeof(int_result));
			return sizeof(int_result);
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
		td->ctlp = cntl_ptr;

		switch(command) {
			case SELECT_BOARD:
				cntl_ptr->ic_cmd = PRIDRViocSEL_BOARD;
				cntl_ptr->ic_len = 1;                
				cntl_ptr->ic_dp = (char *) driver_alloc(10);  /* board string */  
				cntl_ptr->ic_dp[0] = buf[0];          /* board num sent data  */
				break;
#ifdef _LP64 /*  passing a pointer in an ioctl requires 64-bit compilation
                 on 64 bit kernels.  if the kernel is 32 bit or if it is
                 64 bit and the Erlang emulator is built 64 bit this works */
			case BOOT_BOARD:      
				cntl_ptr->ic_cmd = PRIDRViocBOOT;
				cntl_ptr->ic_timout = 60;               /* 60 second timeout   */
				/* we receive a large binary which is the boot image.          */
				/* we must allocate a bootparam structure, initialize it,      */
				/* allocate more space for the binary image and copy the       */
				/* binary image into that space and pass a pointer to the      */
				/* bootparam structure as the data for the ioctl (free later)  */
				td->bp = (download_t *) driver_alloc(sizeof(download_t));
				memset(td->bp, 0, sizeof(download_t));
				cntl_ptr->ic_len = sizeof(download_t);
				cntl_ptr->ic_dp = (char *) td->bp;
				td->bp->len = count;
				td->bp->outptr = (char *) driver_alloc(count);
				if (td->bp->outptr == NULL) {
					free_tdata(td);
					return -1;
				}
				memcpy(td->bp->outptr, buf, count);  /* must make a copy  */
				break;
#endif	/*  _LP64   */
			case ENABLE_MANAGEMENT_CHAN:
				cntl_ptr->ic_cmd = PRIDRViocENA_MGT_CHAN;
				break;
			case RESET_BOARD:
				cntl_ptr->ic_cmd = PRIDRViocRESET_BOARD;
				break;
			case GET_VERSION:
				cntl_ptr->ic_cmd = PRIDRViocGET_VERSION;
				cntl_ptr->ic_len = PRI_VERSION_STRING_LEN;
				cntl_ptr->ic_dp = (char *) driver_alloc(PRI_VERSION_STRING_LEN);
				break;
			case GET_DRIVER_INFO:
				cntl_ptr->ic_cmd = PRIDRViocGET_DRIVER_INFO;
				cntl_ptr->ic_len = sizeof(driver_info_t);
				td->bin = driver_alloc_binary(sizeof(driver_info_t));
				if (td->bin == NULL) {
					free_tdata(td);
					return -1;
				}
				cntl_ptr->ic_dp = (char *) td->bin->orig_bytes;
				break;
			default:
				free_tdata(td);
				return -1;     /*  port_control/3 will exit with badarg  */
		}
		td->ref = driver_async(dd->port, QKEY,
				do_ioctl, td, free_tdata);
		/*  return the reference as a string (list)  */
		sprintf(*res, "%ld", td->ref);
		return(strlen(*res));
	}

	return -1;              /*  port_control/3 will exit with badarg  */

}


/**********************************************************************
 *  netaccess_ready_input                                             *
 *                                                                    *
 *  Erlang/OTP runs this callback when a file decsriptor, previously  *
 *  specified in a driver_select call, has input ready to read.       *
 *  The event argument contains the file descriptor in question.      *
 **********************************************************************/
static void
netaccess_ready_input(ErlDrvData handle, ErlDrvEvent event)
{
	DriverData *dd = (DriverData *) handle;
	ErlDrvBinary *ctrlbin, *databin;
	struct strbuf strctrl, strdata;
	int flags = 0;

fprintf(stderr, "netaccess_ready_input\n\r");
	/*  construct a streams buffer for the control message  */
	ctrlbin = driver_alloc_binary(sizeof(L3_to_L4_struct));
	if (ctrlbin == NULL) {
		driver_failure_posix(dd->port, errno);
		return;
	}
	strctrl.maxlen = ctrlbin->orig_size;
	strctrl.buf = ctrlbin->orig_bytes;

	/*  construct a streams buffer for the data message  */
	databin = driver_alloc_binary(BUFSIZE);
	if (databin == NULL) {
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


/**********************************************************************
 *  netaccess_ready_output                                            *
 *                                                                    *
 *  Erlang/OTP runs this callback when a file decsriptor, previously  *
 *  specified in a driver_select call, is now available to write to.  *
 *  The event argument contains the file descriptor in question.      *
 **********************************************************************/
static void
netaccess_ready_output(ErlDrvData handle, ErlDrvEvent event)
{
	DriverData *dd = (DriverData *) handle;
	int vsize, qsize;
	SysIOVec *iov;

fprintf(stderr, "netaccess_ready_output\n\r");
	while((iov = driver_peekq(dd->port, &vsize)) != NULL) {
		qsize = driver_sizeq(dd->port);
		/*  send the next message to the board  */	
		if(message_to_board(dd->fd, iov) < 0) {
			switch(errno) {
				case EINTR:
				case EAGAIN:
					driver_select(dd->port, (ErlDrvEvent) dd->fd, DO_WRITE, 1);
					return;
				default:
					driver_deq(dd->port, qsize);  /*  dump the whole queue  */
					driver_failure_posix(dd->port, errno);
					return;
			}
		}
		driver_deq(dd->port, iov[0].iov_len); /* dequeue this sent message */
		if(qsize <= dd->low)                /*  queue emptying, unthrottle  */
			set_busy_port(dd->port, 0);
	}
	driver_select(dd->port, (ErlDrvEvent) dd->fd, DO_WRITE, 0);
}


/**********************************************************************
 *  netaccess_finish                                                  *
 *                                                                    *
 *  Erlang/OTP runs this callback when the driver is unloaded with    *
 *  erl_ddll:unload_driver(Name).  Frees all driver data.             *
 **********************************************************************/
static void
netaccess_finish(void) 
{
	
fprintf(stderr, "netaccess_finish\n\r");
}


/**********************************************************************
 *  netaccess_timeout                                                 *
 *                                                                    *
 *  Erlang/OTP runs this callback when a timeout, specified earlier   *
 *  with driver_set_timer, expires.                                   *
 **********************************************************************/
static void
netaccess_timeout(ErlDrvData handle)
{
	DriverData *dd = (DriverData *) handle;
fprintf(stderr, "netaccess_timeout\n\r");

}


/**********************************************************************
 *  netaccess_ready_async                                             *
 *                                                                    *
 *  Erlang/OTP runs this callback when a previously scheduled async   *
 *  operation, called with driver_async, completes.                   *
 **********************************************************************/
static void
netacess_ready_async(ErlDrvData handle, ErlDrvThreadData t_data)
{
	DriverData *dd = (DriverData *) handle;
	ThreadData *td = (ThreadData *) t_data;
	unsigned char ctd[21];	

fprintf(stderr, "netaccess_ready_async\n\r");
	sprintf(ctd, "%ld", td->ref);

	if(td->result < 0) 
		output_atom_result(dd->port, ctd, "error", erl_errno_id(td->errno));
	else
		switch(td->command) {
			case SELECT_BOARD:      
			case BOOT_BOARD:      
			case ENABLE_MANAGEMENT_CHAN:
			case RESET_BOARD:
				output_atom_result(dd->port, ctd, "ok", "done");
				break;
			case GET_VERSION:
				output_string_result(dd->port, ctd, "ok",
						td->ctlp->ic_dp, strlen(td->ctlp->ic_dp));
				break;
			case GET_DRIVER_INFO:
				output_binary_result(dd->port, ctd, "ok",
						td->bin, td->ctlp->ic_len);
				break;
		}
	free_tdata(t_data);
}


/**********************************************************************
 *  netaccess_flush                                                   *
 *                                                                    *
 *  Erlang/OTP runs this callback when there is data in the driver    *
 *  queue and stop is about to be called.                             *
 **********************************************************************/
static void
netaccess_flush(ErlDrvData handle)
{
	DriverData *dd = (DriverData *) handle;

fprintf(stderr, "netaccess_flush\n\r");
}


/**********************************************************************
 *  netaccess_call                                                    *
 *                                                                    *
 *  Erlang/OTP runs this callback in response to a port_call/3.       *
 *  Works mostly like port_control (?).                               *
 **********************************************************************/
static int
netaccess_call(ErlDrvData handle, unsigned int command, 
			char *buf, int len, char **rbuf, int rlen, unsigned int *flags)
{
	DriverData *dd = (DriverData *) handle;

fprintf(stderr, "netaccess_call\n\r");
}


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
	int ret;

fprintf(stderr, "do_iotcl\n\r");
	td->result = ioctl(td->fd, I_STR, (struct strioctl *) td->ctlp);
	if (td->result < 0) 
		td->errno = errno;
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

fprintf(stderr, "free_tdata\n\r");
	if (td->bin != NULL)
		driver_free_binary(td->bin);
	if (td->bp != NULL) {
		if (td->bp->outptr != NULL)
			driver_free(td->bp->outptr);
		driver_free(td->bp);
	}
	if (td->ctlp != NULL) {
		if (td->ctlp->ic_dp != NULL)
			driver_free(td->ctlp->ic_dp);
		driver_free(td->ctlp);
	}
	if (t_data != NULL)
		driver_free(t_data);
}


/*  output result of ioctl to port e.g. {Port, Ref, {ok, Result}}  */
int
output_atom_result(ErlDrvPort port, unsigned char *ref,
				char *ret, char *result)
{
	ErlDrvTermData spec[13 + (strlen(ref) * 2)];
	int i, j;

	i = 0;
	spec[i++] = ERL_DRV_PORT;
	spec[i++] = driver_mk_port(port);

	for(j = 0; j < strlen(ref); j++) {
		spec[i++] = ERL_DRV_INT;
		spec[i++] = ref[j];
	}
	spec[i++] = ERL_DRV_NIL;
	spec[i++] = ERL_DRV_LIST;
	spec[i++] = ++j;

	spec[i++] = ERL_DRV_ATOM;
	spec[i++] = driver_mk_atom(ret);
	spec[i++] = ERL_DRV_ATOM;
	spec[i++] = driver_mk_atom(result);

	spec[i++] = ERL_DRV_TUPLE;
	spec[i++] = 2;
	spec[i++] = ERL_DRV_TUPLE;
	spec[i] = 3;

	return driver_output_term(port, spec, sizeof(spec) / sizeof(spec[0]));
}

/*  output result of ioctl to port e.g. {Port, Ref, {ok, <<Binary>>}}  */
int
output_binary_result(ErlDrvPort port, unsigned char *ref,
				char *ret, ErlDrvBinary *bin, int len)
{
	ErlDrvTermData spec[15 + (strlen(ref) * 2)];
	int i, j;

	i = 0;
	spec[i++] = ERL_DRV_PORT;
	spec[i++] = driver_mk_port(port);

	for(j = 0; j < strlen(ref); j++) {
		spec[i++] = ERL_DRV_INT;
		spec[i++] = ref[j];
	}
	spec[i++] = ERL_DRV_NIL;
	spec[i++] = ERL_DRV_LIST;
	spec[i++] = ++j;

	spec[i++] = ERL_DRV_ATOM;
	spec[i++] = driver_mk_atom(ret);

	spec[i++] = ERL_DRV_BINARY;
	spec[i++] = (ErlDrvTermData) bin;
	spec[i++] = len;
	spec[i++] = 0;

	spec[i++] = ERL_DRV_TUPLE;
	spec[i++] = 2;
	spec[i++] = ERL_DRV_TUPLE;
	spec[i] = 3;

    return driver_output_term(port, spec, sizeof(spec) / sizeof(spec[0]));
}

/*  output result of ioctl to port e.g. {Port, Ref, {ok, "IISDN Ver 7.0"}}  */
int
output_string_result(ErlDrvPort port, unsigned char *ref,
				char *ret, char *string, int len)
{
	ErlDrvTermData spec[14 + (strlen(ref) * 2)];
	int i, j;

	i = 0;
	spec[i++] = ERL_DRV_PORT;
	spec[i++] = driver_mk_port(port);

	for(j = 0; j < strlen(ref); j++) {
		spec[i++] = ERL_DRV_INT;
		spec[i++] = ref[j];
	}
	spec[i++] = ERL_DRV_NIL;
	spec[i++] = ERL_DRV_LIST;
	spec[i++] = ++j;

	spec[i++] = ERL_DRV_ATOM;
	spec[i++] = driver_mk_atom(ret);

	spec[i++] = ERL_DRV_STRING;
	spec[i++] = (ErlDrvTermData) string;
	spec[i++] = len;

	spec[i++] = ERL_DRV_TUPLE;
	spec[i++] = 2;
	spec[i++] = ERL_DRV_TUPLE;
	spec[i] = 3;

    return driver_output_term(port, spec, sizeof(spec) / sizeof(spec[0]));
}


/*  output error e.g.     {Port, {error, einval}}     */
int
output_error(ErlDrvPort port, char *string)
{
	ErlDrvTermData spec[] = {
        	ERL_DRV_PORT, driver_mk_port(port),
        		ERL_DRV_ATOM, driver_mk_atom("error"),
        		ERL_DRV_ATOM, driver_mk_atom(string),
        		ERL_DRV_TUPLE, 2,
        	ERL_DRV_TUPLE, 2,
    };

    return driver_output_term(port, spec, sizeof(spec) / sizeof(spec[0]));
}


/*  output control and data messages e.g. {Port, {<<Control>>,<<Data>>}}  */
int
message_to_port(ErlDrvPort port, ErlDrvBinary *ctrl, int ctrllen,
		ErlDrvBinary *data, int datalen)
{
	ErlDrvTermData spec[] = {
        	ERL_DRV_PORT, driver_mk_port(port),
				ERL_DRV_BINARY, (ErlDrvTermData) ctrl, ctrllen, 0,
				ERL_DRV_BINARY, (ErlDrvTermData) data, datalen, 0,
        		ERL_DRV_TUPLE, 2,
        	ERL_DRV_TUPLE, 2,
    };

    return driver_output_term(port, spec, sizeof(spec) / sizeof(spec[0]));
}


/*  send an SMI message to the board  */
int
message_to_board(int fd, SysIOVec *iov)
{
	struct strbuf ctrlp, datap;

	/*  the first byte is the type; control or data  */
	if(iov->iov_base[0] == 0) {
		ctrlp.len = (iov->iov_len - 1);
		ctrlp.buf = &iov->iov_base[1];
		datap.len = 0;
		datap.buf = NULL;
	} else {
		ctrlp.len = 0;
		ctrlp.buf = NULL;
		datap.len = (iov->iov_len - 1);
		datap.buf = &iov->iov_base[1];
	}

	/*  send the message to the board  */	
	return(putmsg(fd, &ctrlp, &datap, 0));
}
