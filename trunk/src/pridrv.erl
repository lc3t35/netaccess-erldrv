%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% pridrv.erl Erlang module to encode/decode pridrv structures/records %%%
%%%                                                                     %%%
%%%---------------------------------------------------------------------%%%
%%% @copyright Motivity Telecom Inc. 2001-2004                          %%%
%%%                                                                     %%%
%%% @author Vance Shipley <vances@motivity.ca>                          %%%
%%%                                                                     %%%
%%% All rights reserved. No part of this computer program(s) may be     %%%
%%% used, reproduced, stored in any retrieval system, or transmitted,   %%%
%%% in any form or by any means, electronic, mechanical, photocopying,  %%%
%%% recording, or otherwise without prior written permission of         %%%
%%% Motivity Telecom Inc.                                               %%%
%%%---------------------------------------------------------------------%%%
%%%                                                                     %%%
%%% For every structure definition in pridrv.h (e.g. driver_info) we    %%%
%%% have a corresponding record definition (e.g. #driver_info{}) and    %%%
%%% function (e.g. driver_info/1).  The function takes a single         %%%
%%% argument which is either a record or a binary.  The function will   %%%
%%% return a binary given a record or a record given a binary.          %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(pridrv).

-export([driver_info/1]).

-include("pridrv.hrl").

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
