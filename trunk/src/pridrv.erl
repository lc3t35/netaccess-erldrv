%%%---------------------------------------------------------------------
%%% @copyright Motivity Telecom Inc. 2001-2005
%%% @end
%%%
%%% All rights reserved. No part of this computer program(s) may be
%%% used, reproduced, stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of
%%% Motivity Telecom Inc.
%%%---------------------------------------------------------------------
%%%
%%% @author Vance Shipley <vances@motivity.ca>
%%%
%%% @doc Conversion routines for the driver of the netaccess application.
%%%	<p>This module provides functions which convert the binary 
%%% 	format of data passed with IOCTL calls by the linked in driver
%%% 	to the records which are used in the erlang API.  The C language
%%% 	structures defined in the Device Driver Programmer's Manual, and
%%% 	appearing in the <tt>pridrv.h</tt> header file provided with the 
%%% 	board's device drivers, are defined as records in 
%%% 	<tt>pridrv.hrl</tt>.  Each record has a corresponding function
%%% 	with the same name in this module which takes a binary and returns
%%% 	a record.  The same function when given a record will return a
%%% 	properly packed binary which is equivalent to what the C API
%%% 	would have created.  This binary is passed to the board as
%%% 	received by the driver.</p>
%%%
%%% @reference Netaccess&#153; Solaris Device Driver's Manual
%%%

-module(pridrv).
-copyright('Copyright (c) 2001-2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').

-export([driver_info/1]).

-include("pridrv.hrl").

%% @type driver_info(). Statistical information pertaining to a particular board.
%% 	<p>A record which includes the following fields:</p>
%%		<dl>
%%			<dt>board_type</dt> <dd><code>integer()</code></dd>
%%			<dt>hangup_on_red_alarm</dt> <dd><code>integer()</code></dd>
%%			<dt>flow_control_board</dt> <dd><code>integer()</code></dd>
%%			<dt>flow_control_wsrv</dt> <dd><code>integer()</code></dd>
%%			<dt>flow_control_rsrv</dt> <dd><code>integer()</code></dd>
%%			<dt>hdrops</dt> <dd><code>integer()</code></dd>
%%			<dt>sdrops</dt> <dd><code>integer()</code></dd>
%%			<dt>tx_msg_size</dt> <dd><code>integer()</code></dd>
%%			<dt>rx_msg_size</dt> <dd><code>integer()</code></dd>
%%			<dt>tx_num_bufs</dt> <dd><code>integer()</code></dd>
%%			<dt>rx_num_bufs</dt> <dd><code>integer()</code></dd>
%%			<dt>max_data_channels</dt> <dd><code>integer()</code>
%%					maximum number of data channels the driver can support</dd>
%%		</dl>
%%
%% @spec (DriverInfoBin) -> DriverInfoRec
%% 	DriverInfoBin = binary()
%% 	DriverInfoRec = driver_info()
%%
driver_info(R) when is_record(R, driver_info) ->
	<<(R#driver_info.board_type):?SIZEOF_int/native-signed-integer-unit:8,
			(R#driver_info.hangup_on_red_alarm):?SIZEOF_int/native-signed-integer-unit:8,
			(R#driver_info.flow_control_board):?SIZEOF_int/native-signed-integer-unit:8,
			(R#driver_info.flow_control_wsrv):?SIZEOF_int/native-signed-integer-unit:8,
			(R#driver_info.flow_control_rsrv):?SIZEOF_int/native-signed-integer-unit:8,
			(R#driver_info.hdrops):?SIZEOF_int/native-signed-integer-unit:8,
			(R#driver_info.sdrops):?SIZEOF_int/native-signed-integer-unit:8,
			(R#driver_info.tx_msg_size):?SIZEOF_int/native-signed-integer-unit:8,
			(R#driver_info.rx_msg_size):?SIZEOF_int/native-signed-integer-unit:8,
			(R#driver_info.tx_num_bufs):?SIZEOF_ushort/native-unsigned-integer-unit:8,
			(R#driver_info.rx_num_bufs):?SIZEOF_ushort/native-unsigned-integer-unit:8,
			(R#driver_info.max_data_channels):?SIZEOF_uint/native-unsigned-integer-unit:8>>;
driver_info(B) when is_binary(B) ->
	<<BoardType:?SIZEOF_int/native-signed-integer-unit:8,
			HangUpOnRedAlarm:?SIZEOF_int/native-signed-integer-unit:8,
			FlowControlBoard:?SIZEOF_int/native-signed-integer-unit:8,
			FlowControlWsrv:?SIZEOF_int/native-signed-integer-unit:8,
			FlowControlRsrv:?SIZEOF_int/native-signed-integer-unit:8,
			HDrops:?SIZEOF_int/native-signed-integer-unit:8,
			SDrops:?SIZEOF_int/native-signed-integer-unit:8,
			TxMsgSize:?SIZEOF_int/native-signed-integer-unit:8,
			RxMsgSize:?SIZEOF_int/native-signed-integer-unit:8,
			TxNumBufs:?SIZEOF_ushort/native-unsigned-integer-unit:8,
			RxNumBufs:?SIZEOF_ushort/native-unsigned-integer-unit:8,
			MaxDataChannels:?SIZEOF_uint/native-unsigned-integer-unit:8>> = B,
	#driver_info{board_type = BoardType,
			hangup_on_red_alarm = HangUpOnRedAlarm,
			flow_control_board = FlowControlBoard,
			flow_control_wsrv = FlowControlWsrv,
			flow_control_rsrv = FlowControlRsrv,
			hdrops = HDrops,
			sdrops = SDrops,
			tx_msg_size = TxMsgSize,
			rx_msg_size = RxMsgSize,
			tx_num_bufs = TxNumBufs,
			rx_num_bufs = RxNumBufs,
			max_data_channels = MaxDataChannels}.
