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


struct erl_drv_entry netaccess_driver_entry = {
	NULL,							/*  called after loading, redundant        */
	netaccess_start,			/*  called when open_port/2 is invoked,
									    return value -1 means failure          */
	netaccess_stop,			/*  called when port is closed, 
									    and when emulator halted               */
	netaccess_output,			/*  called when we have output from
									    erlang to the port                     */
	netaccess_ready_input,	/*  called when we have input from 
									    one of the driver's handles            */
	netaccess_ready_output,	/*  called when output is possible to
									    one of the driver's handles            */
	"netaccess_drv",				/*  name supplied as command in open_port  */
	netaccess_finish, 		/*  called before unloading the driver     */
	NULL,							/*  handle:  deprecated                    */
	netaccess_control,		/*  invoked by port_command/3              */
	netaccess_timeout,		/*  handling of timeout in driver          */
	netaccess_outputv,		/*  called when we have output from
									    erlang to the port                     */
	netacess_ready_async,	/*  ready on the async driver              */
	netaccess_flush,			/*  called when the port is about to be 
									 	 closed, and there is data in the driver
										 queue that needs to be flushed before
										 'stop' can be called                   */
	netaccess_call				/*  called when port_call/3 is invoked     */
};

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

	/*  unregister the device handle  */
	driver_select(dd->port, (ErlDrvEvent) dd->fd, DO_READ, 0);
	driver_select(dd->port, (ErlDrvEvent) dd->fd, DO_WRITE, 0);

	close(dd->fd);
	driver_free(dd);
	/*  what about any thread data that might still be kicking around?  */
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

}


/**********************************************************************
 *  netaccess_oputputv                                                *
 *                                                                    *
 *  Erlang/OTP runs this callback in response to a port_command/2.    *
 *  Supports scatter/gather IO.  When defined in driver_entry this    *
 *  function takes precendence over output (netaccess_output).        *
 *  [Note:  the first element of the IO vector is null so we skip to  *
 *          the second and start from there.  This is done to make it *
 *          easier for the inet driver to add in a header.]           *
 *  Receives data from an Erlang process.                             *
 **********************************************************************/
static void
netaccess_outputv(ErlDrvData handle, ErlIOVec *ev)
{
	DriverData *dd = (DriverData *) handle;
	int sz;

	if(ev->vsize != 3) {
		output_error("einval");
		return;
	}

	/*  if there's a queue just add to it  */
	if((sz = driver_sizeq(dd->port)) > 0) {
			driver_enqv(dd->port, ev, 0);
			if((sz + 1) >= dd->high)           /*  queue full, throttle  */
				set_busy_port(dd->port, 1);
			return;
	}

	/*  send the message to the board  */	
	if(message_to_board(dd->fd, ev->iov) < 0) {
		if(errno == EAGAIN) {
			driver_enqv(dd->port, ev, 0);
			driver_select(dd->port, (ErlDrvEvent) dd->fd, DO_WRITE, 1);
		} else {
			output_error(erl_errno_id(errno));
			return;
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

	switch(command) {
		case CANCEL_ASYNC:
			int_result = driver_async_cancel(*buf);
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
#ifdef _LP64	/*  passing a pointer in an ioctl requires 64-bit compilation  */
			case BOOT_BOARD:      
				cntl_ptr->ic_cmd = PRIDRViocBOOT;
				cntl_ptr->ic_timout = 60;             /* 60 second timeout   */
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
		memcpy(*res, &td->ref, sizeof(td->ref));
		return sizeof(td->ref);
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
	char *databuf;

	/*  construct a streams buffer for the control message  */
	ctrlbin = driver_alloc_binary(sizeof(L4_to_L3_struct));
	strctrl.maxlen = sizeof(L4_to_L3_struct);
	strctrl.len = 0;
	strctrl.buf = ctrlbin->orig_bytes;

	/*  construct a streams buffer for the data message  */
	databin = driver_alloc_binary(BUFSIZE);
	strdata.maxlen = BUFSIZE;
	strdata.len = 0;
	strdata.buf = databin->orig_bytes;

	/*  read a message from the board  */
	if(getmsg(dd->fd, &strctrl, &strdata, 0) < 0) {
		output_error(erl_errno_id(errno));
		return;
	}

	/*  send the control & data messages to the port owner  */
	message_to_port(dd->port, ctrlbin, databin);

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

	while((iov = driver_peekq(dd->port, &vsize)) != NULL) {
		if(vsize < 3) {
			qsize = driver_sizeq(dd->port);
			driver_deq(dd->port, qsize);   /*  dump the whole queue  */     
			output_error("einval");
			return;
		}
		/*  send the message to the board  */	
		if(message_to_board(dd->fd, iov) < 0) {
			if(errno == EAGAIN) {
				driver_select(dd->port, (ErlDrvEvent) dd->fd, DO_WRITE, 1);
				return;
			} else {
				qsize = driver_sizeq(dd->port);
				driver_deq(dd->port, 3);      /*  dump this message  */     
				output_error(erl_errno_id(errno));
				continue;
			}
		}
		driver_deq(dd->port, 3);        /*  dequeue this sent message  */
		qsize = driver_sizeq(dd->port);
		if(qsize <= dd->low)            /*  queue emptying, unthrottle  */
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
	unsigned char ctd[4];	

	/*  TODO:  find a portable way to handle this!  */
	/*  convert the async refernce (a long) to a list unsigned chars  */
	memcpy(ctd, &td->ref, sizeof(ctd));

	if(td->result < 0) 
		output_atom_result(dd->port, ctd[0], ctd[1], ctd[2],
				 ctd[3], "error", erl_errno_id(td->errno));
	else
		switch(td->command) {
			case SELECT_BOARD:      
			case BOOT_BOARD:      
			case ENABLE_MANAGEMENT_CHAN:
			case RESET_BOARD:
				output_atom_result(dd->port, ctd[0],  ctd[1],  ctd[2],
					ctd[3], "ok", "done");
				break;
			case GET_VERSION:
				output_string_result(dd->port, ctd[0], ctd[1], ctd[2],
						ctd[3], "ok", td->ctlp->ic_dp, strlen(td->ctlp->ic_dp));
				break;
			case GET_DRIVER_INFO:
				output_binary_result(dd->port, ctd[0], ctd[1], ctd[2],
						ctd[3], "ok", td->bin, td->ctlp->ic_len);
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
output_atom_result(ErlDrvPort port, int a, int b, int c, int d,
				char *ret, char *result)
{
	ErlDrvTermData spec[] = {
        	ERL_DRV_PORT, driver_mk_port(port),
        		ERL_DRV_INT, a,
        		ERL_DRV_INT, b,
        		ERL_DRV_INT, c,
        		ERL_DRV_INT, d,
				ERL_DRV_NIL,
        	ERL_DRV_LIST, 5,
        		ERL_DRV_ATOM, driver_mk_atom(ret),
				ERL_DRV_ATOM, driver_mk_atom(result),
        	ERL_DRV_TUPLE, 2,
        	ERL_DRV_TUPLE, 3,
    };

    return driver_output_term(port, spec, sizeof(spec) / sizeof(spec[0]));
}

/*  output result of ioctl to port e.g. {Port, Ref, {ok, <<Binary>>}}  */
int
output_binary_result(ErlDrvPort port, int a, int b, int c, int d,
				char *ret, ErlDrvBinary *bin, int len)
{
	ErlDrvTermData spec[] = {
        	ERL_DRV_PORT, driver_mk_port(port),
        		ERL_DRV_INT, a,
        		ERL_DRV_INT, b,
        		ERL_DRV_INT, c,
        		ERL_DRV_INT, d,
				ERL_DRV_NIL,
        	ERL_DRV_LIST, 5,
        		ERL_DRV_ATOM, driver_mk_atom(ret),
			ERL_DRV_BINARY, (ErlDrvTermData) bin, len, 0,
        		ERL_DRV_TUPLE, 2,
        	ERL_DRV_TUPLE, 3,
    };

    return driver_output_term(port, spec, sizeof(spec) / sizeof(spec[0]));
}

/*  output result of ioctl to port e.g. {Port, Ref, {ok, "IISDN Ver 7.0"}}  */
int
output_string_result(ErlDrvPort port, int a, int b, int c, int d,
				char *ret, char *string, int len)
{
	ErlDrvTermData spec[] = {
        	ERL_DRV_PORT, driver_mk_port(port),
        		ERL_DRV_INT, a,
        		ERL_DRV_INT, b,
        		ERL_DRV_INT, c,
        		ERL_DRV_INT, d,
				ERL_DRV_NIL,
        	ERL_DRV_LIST, 5,
        		ERL_DRV_ATOM, driver_mk_atom(ret),
			ERL_DRV_STRING, (ErlDrvTermData) string, len,
        		ERL_DRV_TUPLE, 2,
        	ERL_DRV_TUPLE, 3,
    };

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
message_to_port(ErlDrvPort port, ErlDrvBinary *ctrl, ErlDrvBinary *data)
{
	ErlDrvTermData spec[] = {
        	ERL_DRV_PORT, driver_mk_port(port),
				ERL_DRV_BINARY, (ErlDrvTermData) ctrl, ctrl->orig_size, 0,
				ERL_DRV_BINARY, (ErlDrvTermData) data, data->orig_size, 0,
        		ERL_DRV_TUPLE, 2,
        	ERL_DRV_TUPLE, 3,
    };

    return driver_output_term(port, spec, sizeof(spec) / sizeof(spec[0]));
}


/*  send an SMI message to the board  */
int
message_to_board(int fd, SysIOVec *iov)
{
	struct strbuf ctrlp, datap;

	/*  construct the streams buffers  */
	memset(&ctrlp, 0, sizeof(ctrlp));
	memset(&datap, 0, sizeof(datap));
	ctrlp.buf = iov[1].iov_base;
	ctrlp.len = iov[1].iov_len;
	datap.buf = iov[2].iov_base;
	datap.len = iov[2].iov_len;

	/*  send the message to the board  */	
	return (putmsg(fd, &ctrlp, &datap, 0));
}
