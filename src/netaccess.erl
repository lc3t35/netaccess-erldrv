%%%---------------------------------------------------------------------
%%% Copyright Motivity Telecom Inc. 2001, 2002
%%%
%%% All rights reserved. No part of this computer program(s) may be
%%% used, reproduced, stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of
%%% Motivity Telecom Inc.
%%%---------------------------------------------------------------------
         
-module(netaccess).
-copyright('Copyright (c) 2001, 2002 Motivity Telecom Inc.').
-vsn('0.1').
-author('vances@motivity.ca').

-behaviour(gen_server).

%% call backs needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%% our published API functions
-export([start/0,start_link/0,stop/0]).
-export([open/0,open/1,close/1]).
-export([select_board/2,boot/2,enable_management_chan/1,
			reset_board/1,get_version/1, get_driver_info/1]).
-export([set_hardware/2, set_hardware/3, set_hardware/4, req_hw_status/1]).

% test
-export([mergeopts/2]).


%% ioctl commands
-define(BOOT_BOARD, 0).
-define(ENABLE_MANAGEMENT_CHAN, 1).
-define(RESET_BOARD,  2).
-define(GET_VERSION,  3).
-define(GET_DRIVER_INFO, 4).
-define(SELECT_BOARD, 5).
-define(CANCEL_ASYNC, 10).

-define(PRIs8bit, 8/?ENDIANESS-signed-integer).
-define(PRIu8bit, 8/?ENDIANESS-unsigned-integer).
-define(PRIs16bit, 16/?ENDIANESS-signed-integer).
-define(PRIu16bit, 16/?ENDIANESS-unsigned-integer).
-define(PRIs32bit, 32/?ENDIANESS-signed-integer).
-define(PRIu32bit, 32/?ENDIANESS-unsigned-integer).

-define(PRI_MAX_LINES, 8).

%%
%% defines for get_driver_info/2
%%
-define(DriverInfoMask,
		<<BoardType:?SIZEINT/?ENDIANESS-signed-integer,
		HangUpOnRedAlarm:?SIZEINT/?ENDIANESS-signed-integer,
		FlowControlBoard:?SIZEINT/?ENDIANESS-signed-integer,
		FlowControlWsrv:?SIZEINT/?ENDIANESS-signed-integer,
		FlowControlRsrv:?SIZEINT/?ENDIANESS-signed-integer,
		HDrops:?SIZEINT/?ENDIANESS-signed-integer,
		SDrops:?SIZEINT/?ENDIANESS-signed-integer,
		TxMsgSize:?SIZEINT/?ENDIANESS-signed-integer,
		RxMsgSize:?SIZEINT/?ENDIANESS-signed-integer,
		TxNumBufs:?SIZEUSHORT/?ENDIANESS-unsigned-integer,
		RxNumBufs:?SIZEUSHORT/?ENDIANESS-unsigned-integer,
		MaxDataChannels:?SIZEUINT/?ENDIANESS-unsigned-integer>>).
-define(DriverInfoTerms,
		[{board_type, BoardType}, {hangup_on_red_alarm, HangUpOnRedAlarm},
		{flow_control_board, FlowControlBoard},
		{flow_control_wsrv, FlowControlWsrv},
		{flow_control_rsrv, FlowControlRsrv},
		{hdrops, HDrops}, {sdrops, SDrops},
		{tx_msg_size, TxMsgSize}, {rx_msg_size, RxMsgSize},
		{tx_num_bufs, TxNumBufs},	{rx_num_bufs, RxNumBufs},
		{max_data_channels, MaxDataChannels}]).

%%
%% defines for the L4L3 & L3L4 Common Headers 
%%
-define(L4L3_Mask(LapdId, MsgType, L4Ref, CallRef, Lli),
		<<0, LapdId:?PRIu8bit, MsgType:?PRIu8bit, L4Ref:?PRIu16bit,
		CallRef:?PRIu16bit, Lli:?PRIu16bit>>).
-define(L3L4_Mask(LapdId, MsgType, L4Ref, CallRef, BChan,
		Iface, BChanMask, Lli, DataChan),
		<<LapdId:?PRIu8bit, MsgType:?PRIu8bit, L4Ref:?PRIu16bit,
		CallRef:?PRIu16bit, BChan:?PRIu8bit, Iface:?PRIu8bit,
		 BChanMask:?PRIu32bit, Lli:?PRIu16bit, DataChan:?PRIu16bit>>).

%%
%% defines for set_hardware/2 and req_hw_status/1
%%
-define(L4L3mSET_HARDWARE, 16#A7). 
-define(L4L3mREQ_HW_STATUS, 16#A8). 
-define(L3L4mHARDWARE_STATUS, 16#24). 
-define(PRI_HARDWARE_DATA,
		<<_:?PRIu8bit, ?L3L4mHARDWARE_STATUS:?PRIu8bit,
		_:(14*8), HardwareBin:12/binary,
		LineBins:(16*?PRI_MAX_LINES)/binary,
		CsuBin:(?PRI_MAX_LINES)/binary, _/binary>>).
-define(HardwareMask, <<Clocking:?PRIu8bit, Clocking2:?PRIu8bit,
		EnableClocking2:?PRIu8bit, NetRefClocking:?PRIu8bit,
		NetRefRate:?PRIu8bit, CtBusMode:?PRIu8bit,
		ForceFramerInit:?PRIu8bit, TdmRate:?PRIu8bit,
		Enable8370RliuMonitor:?PRIu8bit, DbCount:?PRIu8bit,
		EnableT810xSnapMode:?PRIu8bit, ClkStatus:?PRIu8bit>>).
-define(HardwareTerms, [{clk_status,ClkStatus}, {clocking,Clocking},
		{clocking2,Clocking2}, {ctbus_mode,CtBusMode},
		{dbcount,DbCount}, {enable_8370_rliu_monitor,Enable8370RliuMonitor},
		{enable_clocking2,EnableClocking2},
		{enable_t810x_snap_mode,EnableT810xSnapMode},
		{force_framer_init,ForceFramerInit},
		{netref_clocking,NetRefClocking}, {netref_rate,NetRefRate},
		{tdm_rate,TdmRate}]).
-define(HardwareDefaults,
		[{clk_status,0}, {clocking,0}, {clocking2,0}, {ctbus_mode,0},
		{dbcount,0}, {enable_8370_rliu_monitor,0}, {enable_clocking2,0},
		{enable_t810x_snap_mode,0}, {force_framer_init,0},
		{netref_clocking,0}, {netref_rate,0}, {tdm_rate,0}]).
-define(LineMask, <<Framing:?PRIu8bit, LineCode:?PRIu8bit, PmMode:?PRIu8bit,
		LineLength:?PRIu8bit, Term:?PRIu8bit, LineType:?PRIu8bit,
		IntegrateAlarms:?PRIu8bit, FilterUnsolicited:?PRIu8bit,
		0:?PRIu8bit, FilterYellow:?PRIu8bit, BriL1Mode:?PRIu8bit,
		BriL1Cmd:?PRIu8bit, BriLoop:?PRIu8bit, BriL1T3:?PRIu8bit,
		BriL1T4:?PRIu16bit>>).
-define(LineTerms,
		[{briL1_T3, BriL1T3}, {briL1_T4, BriL1T4}, {briL1_cmd, BriL1Cmd},
		{bri_l1mode, BriL1Mode}, {bri_loop, BriLoop},
		{filter_unsolicited, FilterUnsolicited},
		{filter_yellow, FilterYellow}, {framing, Framing},
		{integrate_alarms, IntegrateAlarms},
		{line_code, LineCode}, {line_length, LineLength},
		{line_type, LineType}, {pm_mode, PmMode}, {term, Term}]).
-define(LineDefaults, 
		[{briL1_T3,0}, {briL1_T4,0}, {briL1_cmd,0}, {bri_l1mode,0},
		{bri_loop,0}, {filter_unsolicited,0}, {filter_yellow,0},
		{framing,0}, {integrate_alarms,0}, {line_code,0},
		{line_length,0}, {line_type,0}, {pm_mode,0}, {term,0}]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% This module implements the Erlang side of the netaccess device driver
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%----------------------------------------------------------------------
%%  The gen_server call backs
%%----------------------------------------------------------------------

%% 
%% start the netacccess server
%%
%% returns {ok, Pid} or {error, Reason} (or ignore?)
%%
start() ->
	gen_server:start({local, netaccess_server}, ?MODULE, [], []).
	
start_link() ->
	gen_server:start_link({local, netaccess_server}, ?MODULE, [], []).


%%
%% stop the netacccess server
%%
%% returns ok
%%
stop() ->
	do_call(stop).


%%
%% open a channel on a netaccess board
%%
%% returns {ok, Port} or {error, Reason}
%%
open() ->
	do_call({open, "netaccess_drv"}).

open(Board) when list(Board) ->
	do_call({open, "netaccess_drv " ++ Board});
open(Board) when atom(Board) ->
	do_call({open, "netaccess_drv " ++ atom_to_list(Board)}).


%%
%% close a channel on a netaccess board
%%
%% returns ok or {error, Reason}
%%
close(Port) ->
	do_call({close, Port}).


%%
%% select a netaccess board for the channel 
%%
%% returns {ok, done} or {error, Reason}
%%
select_board(Port, Board) when integer(Board) ->
	do_ioctl({ioctl, ?SELECT_BOARD, [Board], Port}).


%%
%% enable a management channel on an open netaccess board
%%
%% returns {ok, done} or {error, Reason}
%%
enable_management_chan(Port) ->
	do_ioctl({ioctl, ?ENABLE_MANAGEMENT_CHAN, [], Port}).


%%
%% boot an open netaccess board
%%
%% returns {ok, done} or {error, Reason}
%%
boot(Port, BootBin) when binary(BootBin) ->
	do_ioctl({ioctl, ?BOOT_BOARD, BootBin, Port}, 62000);
boot(Port, Filename) when list(Filename) ->
	case catch file:read_file(Filename) of
		{ok, BootBin} ->
			boot(Port, BootBin);
		{error, Reason} ->
			{error, Reason}
	end.


%%
%% perform a reset on an open netaccess board
%%
%% returns {ok, done} or {error, Reason}
%%
reset_board(Port) ->
	do_ioctl({ioctl, ?RESET_BOARD, [], Port}).


%%
%% get software version string from an open netaccess board
%%
%% returns {ok, VersionString} or {error, Reason}
%%
get_version(Port) ->
	do_ioctl({ioctl, ?GET_VERSION, [], Port}).


%%
%% get driver information from an open netaccess board
%%
%% returns {ok, <<DriverInfo>>} or {error, Reason}
%%
get_driver_info(Port) ->
	case do_ioctl({ioctl, ?GET_DRIVER_INFO, [], Port}) of
		{ok, ?DriverInfoMask} -> {ok, ?DriverInfoTerms};
		Return -> Return
	end.
	


% may be called without line settings and CSU flags, we'll use defaults
set_hardware(Port, HardwareSettings) ->
	set_hardware(Port, HardwareSettings, [{framing, 0}], 0).

% may be called without CSU flags in which case we'll use defaults
set_hardware(Port, HardwareSettings, LineSettings) ->
	set_hardware(Port, HardwareSettings, LineSettings, 0).

% may be called with only one CSU flag in which case we will
% use this setting for all spans
set_hardware(Port, HardwareSettings, LineSettings, CsuFlag)
		when integer(CsuFlag) ->
	set_hardware(Port, HardwareSettings, LineSettings,
			lists:duplicate(?PRI_MAX_LINES, CsuFlag));

% may be called with only one list of line settings in which
% case we will use these settings for all spans
set_hardware(Port, HardwareSettings, LineSettings, CsuFlags) 
		when list(LineSettings), tuple(hd(LineSettings)) ->
	set_hardware(Port, HardwareSettings,
			lists:duplicate(?PRI_MAX_LINES, LineSettings), CsuFlags);

% may be called with an unordered subset of settings
set_hardware(Port, HardwareSettings, LineSettings, CsuFlags) 
		when list(LineSettings), length(LineSettings) == ?PRI_MAX_LINES,
		list(CsuFlags), length(CsuFlags) == ?PRI_MAX_LINES ->
	HardwareDefaults = ?HardwareDefaults,
	LineDefaults = ?LineDefaults,
	HardwareMerged = lists:keysort(1, mergeopts(HardwareSettings,
			HardwareDefaults)),
	LineMerged = [lists:keysort(1, mergeopts(A, B)) ||
			A <- LineSettings, B <- [LineDefaults]],
	do_set_hardware(Port, HardwareMerged, LineMerged, CsuFlags).
	
%%
%% internal function to set the hardware options
%%
do_set_hardware(Port, ?HardwareTerms, LineSettings, CsuFlags) ->
	HardwareBin = ?HardwareMask,
	MakeLineBin = fun(?LineTerms) -> ?LineMask end,
	LineBins = [MakeLineBin(A) || A <- LineSettings],
	CsuBin = list_to_binary(CsuFlags),
	L4L3_Bin = ?L4L3_Mask(0, ?L4L3mSET_HARDWARE, 0, 0, 0),
	L4_to_L3_struct = concat_binary([L4L3_Bin, HardwareBin, LineBins, CsuBin]),
	port_command(Port, L4_to_L3_struct),
	receive 
		{Port, {error, Reason}} -> {error, Reason}
		after 200 -> {ok, done}
	end.


%%
%% query the hardware setup
%%
%% returns {ok, [HardwareTerms], [[LineTerms]|...], CsuTerms}
%%
req_hw_status(Port) ->
	L4L3_Bin = ?L4L3_Mask(0, ?L4L3mREQ_HW_STATUS, 0, 0, 0),
	port_command(Port, L4L3_Bin),
	receive 
		{Port, {error, Reason}} -> {error, Reason};
		{Port, {?PRI_HARDWARE_DATA, DataBin}} ->
			?HardwareMask = HardwareBin,
			{ok, ?HardwareTerms, req_hw_status1(LineBins,
					size(LineBins) div ?PRI_MAX_LINES, []),
					binary_to_list(CsuBin)}; 
		{Port, {error, Reason}} -> {error, Reason}
		after 200 -> {error, timeout}
	end.
req_hw_status1(<<>>, LineBinSize, LineTerms) -> LineTerms;
req_hw_status1(LineBins, LineBinSize, LineTerms) ->
	{?LineMask, Rest} = split_binary(LineBins, LineBinSize),
	req_hw_status1(Rest, LineBinSize, LineTerms ++ ?LineTerms).
	
	
	

%%----------------------------------------------------------------------
%%  The gen_server call backs
%%----------------------------------------------------------------------

%% initialize the netaccess server
init([]) ->
	process_flag(trap_exit, true),
	erl_ddll:start(),
	% load the dynamicly linked device driver
	PrivDir = code:priv_dir(netaccess),
	LibDir = filename:join([PrivDir, "lib"]),
	case erl_ddll:load_driver(LibDir, netaccess_drv) of
		{error, ErrorDescriptor} ->
			{stop, erl_ddll:format_error(ErrorDescriptor)};
		ok ->
			{ok, []}
	end.


%% open a port to the netaccess board
handle_call({open, Board}, {Pid, _Tag}, State) ->
	case catch erlang:open_port({spawn, Board}, []) of
		{'EXIT', Reason} ->
			Reply = {error, Reason};
		Port when port(Port) ->
			case catch erlang:port_connect(Port, Pid) of
				true ->
					unlink(Port),
					Reply = {ok, Port};
				{'EXIT', Reason} ->
					Reply = {error, Reason}
			end
	end,
	{reply, Reply, State};


%% close a port on a netaccess board
handle_call({close, Port}, _From, State) ->
	case catch erlang:port_close(Port) of
		{'EXIT', Reason} ->
			Reply = {error, Reason};
		true -> Reply = ok
	end,
	{reply, Reply, State};
	

%% perform an ioctl on an open channel to a netaccess board
handle_call({ioctl, Operation, Data, Port}, _From, State) ->
	case catch erlang:port_control(Port, Operation, Data) of
		{'EXIT', Reason} ->
			error_logger:error_msg('netaccess failed call operation'),
			Reply = {error, Reason};
		Ref ->
			Reply = {ok, Ref}
	end,
	{reply, Reply, State};


%% shutdown the netaccess server
handle_call(stop, _From, State) ->
	{stop, shutdown, ok, State};

handle_call(_, _, State) ->
	{noreply, State}.

handle_cast({ioctl, Operation, Data, Port}, State) ->
	case catch erlang:port_control(Port, Operation, Data) of
		{'EXIT', Reason} ->
			error_logger:error_msg('netaccess failed cast operation');
		_ -> ok
	end,
	{noreply, State};
	
handle_cast(_, State) ->
	{noreply, State}.

handle_info({'EXIT', Port, Reason}, State) ->
	error_logger:error_msg('Port controlling netaccess_drv terminated'),
	{stop, normal, State};

handle_info(Unknown, State) ->
	io:format("~p~n", [Unknown]),
	{noreply, State}.

% someone wants us to shutdown and cleanup
terminate(_Reason, State) ->
	case catch erl_ddll:unload_driver(netaccess_drv) of
		{'EXIT', Error} ->
			error_logger:error_msg('failed to unload netaccess_drv');
		Return -> Return
	end.

code_change(_, _, _) -> ok.


%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

do_ioctl(Request) ->
	do_ioctl(Request, 20000).
do_ioctl({ioctl, Operation, Data, Port}, Timeout) ->
	case do_call({ioctl, Operation, Data, Port}) of
		{error, Reason} ->
			{error, Reason};
		{ok, Ref} -> 
			receive
				{Port, Ref, Result} -> Result
				after Timeout ->
					do_cast({ioctl, ?CANCEL_ASYNC, Ref, Port}),
					{error, timeout}
			end
	end.

do_call(Request) ->
	Server = 
		case whereis(netaccess_server) of
			undefined -> {ok, Pid} = start(), Pid;
			Pid -> Pid
		end,
	gen_server:call(Server, Request).

do_cast(Request) ->
	Server = 
		case whereis(netaccess_server) of
			undefined -> {ok, Pid} = start(), Pid;
			Pid -> Pid
		end,
	gen_server:cast(Server, Request).

% merges a list of option settings with the list of default values
mergeopts([], Merged) -> Merged;
mergeopts([{Option, Value}|T], Defaults) ->
	Merged = lists:keyreplace(Option, 1, Defaults, {Option, Value}),
	mergeopts(T, Merged).
