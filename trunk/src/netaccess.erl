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
-export([set_tsi/3, req_tsi_status/1]).

-include("pridrv.hrl").
-include("naii.hrl").


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
%% returns {ok, Port} or fails
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
%% returns true
%%
close(Port) ->
	do_call({close, Port}).


%%
%% select a netaccess board for the channel 
%%
%% returns ok or fails
%%
select_board(Port, Board) when integer(Board) ->
	do_ioctl({ioctl, ?SELECT_BOARD, Board, Port}).


%%
%% enable a management channel on an open netaccess board
%%
%% returns ok or fails
%%
enable_management_chan(Port) ->
	do_ioctl({ioctl, ?ENABLE_MANAGEMENT_CHAN, [], Port}).


%%
%% boot an open netaccess board
%%
%% returns ok or {error, Reason}
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
%% returns ok or fails
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
%% returns {ok, DriverInfo} or {error, Reason}
%%
%%   DriverInfo = [{item, Value}, ...]
%%   item       = hangup_on_red_alarm | flow_control_board |
%%                   flow_control_wsrv | flow_control_rsrv |
%%                   hdrops | sdrops | tx_msg_size | rx_msg_size |
%%                   tx_num_bufs | rx_num_bufs | max_data_channels
%%   Value      = int()
%%
get_driver_info(Port) ->
	case do_ioctl({ioctl, ?GET_DRIVER_INFO, [], Port}) of
		{ok, ?DriverInfoMask} -> {ok, ?DriverInfoTerms};
		Return -> Return
	end.
	

%%
%% set hardware settings on board such as clocking, framing
%%
%% returns true or fails
%% 

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
	?HardwareTerms = HardwareMerged,
	HardwareBin = ?HardwareMask,
	LineMerged = [lists:keysort(1, mergeopts(A, B)) ||
			A <- LineSettings, B <- [LineDefaults]],
	MakeLineBin = fun(?LineTerms) -> ?LineMask end,
	LineBins = [MakeLineBin(A) || A <- LineMerged],
	CsuBin = list_to_binary(CsuFlags),
	L4_Ref = 16#FFFF,
	L4L3_Bin = ?L4L3_Mask(0, ?L4L3mSET_HARDWARE, L4_Ref, 0, 0),
	L4_to_L3_struct = concat_binary([L4L3_Bin, HardwareBin, LineBins, CsuBin]),
	do_call({'L4L3m', Port, L4_Ref, L4_to_L3_struct}).


%%
%% query the hardware setup
%%
%% returns {ok, [HardwareTerms], [[LineTerms]|...], CsuTerms}
%%
req_hw_status(Port) ->
	L4L3_Bin = ?L4L3_Mask(0, ?L4L3mREQ_HW_STATUS, 16#FFFF, 0, 0),
	port_command(Port, L4L3_Bin),
	receive 
		{Port, {'L3L4m', ?L3L4_Mask(_LapdId, ?L3L4mHARDWARE_STATUS,
				_L4Ref, _CallRef, _BChan, _IFace, _BChanMask, _Lli,
				_DataChan, Rest), _DataBin}} ->
			?PRI_HARDWARE_DATA = Rest,
			?HardwareMask = HardwareBin,
			{ok, ?HardwareTerms, req_hw_status(LineBins,
					size(LineBins) div ?PRI_MAX_LINES, []),
					binary_to_list(CsuBin)}; 
		{Port, {error, Reason}} -> {error, Reason}
	after
		1000 -> {error, timeout}
	end.
req_hw_status(<<>>, LineBinSize, LineTerms) -> LineTerms;
req_hw_status(LineBins, LineBinSize, LineTerms) ->
	{?LineMask, Rest} = split_binary(LineBins, LineBinSize),
	req_hw_status(Rest, LineBinSize, LineTerms ++ ?LineTerms).
	
	
%%
%% create timeslot mappings
%%
set_tsi(Port, Granularity, TsiMapTerms) ->
	set_tsi(Port, length(TsiMapTerms), Granularity, TsiMapTerms, <<>>).
set_tsi(Port, NumMappings, Granularity, [], TsiMapBins) ->
	L4L3_Bin = ?L4L3_Mask(0, ?L4L3mSET_TSI, 16#FFFF, 0, 0),
	Last = 0,
	L4_to_L3_struct = concat_binary([L4L3_Bin, ?PRI_TSI_DATA]),
	port_command(Port, L4_to_L3_struct),
	receive 
		{Port, {error, Reason}} -> {error, Reason};
		{Port, {?L3L4_Mask(_LapdId, ?L3L4mERROR, _L4Ref, _CallRef,
				_BChan, _IFace, _BChanMask, _Lli, _DataChan, Rest), _DataBin}} ->
			<<Error:?PRIu8bit, _/binary>> = Rest,
			{error, ?L3L4mErrorMsg(Error)}
	after
		100 -> ok
	end;
set_tsi(Port, NumMappings, Granularity, [?TsiMapTerms | T], TsiMapBins) ->
	set_tsi(Port, NumMappings, Granularity, T,
			concat_binary([TsiMapBins, ?TsiMapMask])).


%%
%% query the timeslot mappings
%%
req_tsi_status(Port) ->
	L4L3_Bin = ?L4L3_Mask(0, ?L4L3mREQ_TSI_STATUS, 16#FFFF, 0, 0),
	port_command(Port, L4L3_Bin),
	receive 
		{Port, {'L3L4m', ?L3L4_Mask(_LapdId, ?L3L4mTSI_STATUS,
				_L4Ref, _CallRef, _BChan, _IFace, _BChanMask,
				_Lli, _DataChan, Rest), _DataBin}} ->
			?PRI_TSI_DATA = Rest,
			{ok, {num_mappings, NumMappings},
					{granularity, Granularity}, {last, Last},
					req_tsi_status(TsiMapBins, NumMappings, [])};
		{Port, {error, Reason}} -> {error, Reason}
	after
		1000 -> {error, timeout}
	end.
req_tsi_status(TsiMapBins, 0, TsiTerms) -> TsiTerms;
req_tsi_status(TsiMapBins, NumMaps, TsiTerms) ->
	{?TsiMapMask, Rest} = split_binary(TsiMapBins, 4),
	req_tsi_status(Rest, NumMaps - 1, TsiTerms ++ ?TsiMapTerms).
	

%%
%% enable_protocol(Port, Command, CommandParameter, Level1,
%%                 Level2, Level3) ->  
%% 
%%    Command          = int()
%%    CommandParameter = int()
%%    Level1           = record() of type pri_level1_config
%%    Level2           = record() of type pri_level2_config
%%    Level3           = record() of type pri_level3_config
%%
%% Use the naii library to decode/encode these records and the
%% binaries received/sent to the boards.
%%
enable_protocol(Port, Command, CommandParameter,
		Level1, Level2, Level3) -> ok.


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
			{ok, gb_trees:empty()}
	end.


%% open a port to the netaccess board
handle_call({open, Board}, {Pid, _Tag}, State) ->
	case catch erlang:open_port({spawn, Board}, [binary]) of
		Port when is_port(Port) -> 
			NewState = gb_trees:insert({port, Port}, {Pid, now()}, State),
			{reply, {ok, Port}, NewState};
		Error ->
			{reply, Error, State}
	end;

%% close a port on a netaccess board
handle_call({close, Port}, _From, State) ->
	NewState = clean_port(Port, State),
	{reply, catch erlang:port_close(Port), NewState};

%% perform an ioctl on an open channel to a netaccess board
handle_call({ioctl, Operation, Data, Port}, From, State) ->
	case catch erlang:port_call(Port, Operation, Data) of
		{ok, Ref} ->
			NewState = gb_trees:insert({ref, Ref},
					{Port, From, now()}, State),
			{noreply, NewState};
		Error ->
			{reply, Error, State}
	end;

%% send an SMI message to the board
handle_call({'L4L3m', Port, L4_Ref, L4L3_Msg}, From, State) ->
	case catch erlang:port_command(Port, L4L3_Msg) of
		true ->
			{reply, true, State};
		Error ->
			{reply, Error, State}
	end;

%% shutdown the netaccess server
handle_call(stop, _From, State) ->
	{stop, shutdown, ok, State};

handle_call(_, _, State) ->
	{noreply, State}.

handle_cast({ioctl, Operation, Data, Port}, State) ->
	catch erlang:port_call(Port, Operation, Data),
	{noreply, State};
	
handle_cast(_, State) ->
	{noreply, State}.

% an asynch task has completed
handle_info({Port, {ref, Ref}, Result}, State) when is_port(Port) ->
	{Port, From, _Time} = gb_trees:get({ref, Ref}, State),
	NewState = gb_trees:delete({ref, Ref}, State),
	gen_server:reply(From, Result),
	{noreply, NewState};

% an L3L4 SMI control message has arrived from the board
handle_info({Port, {'L3L4m', ?L3L4_Mask(LapdId, MsgType, L4Ref,
		CallRef, BChan, Iface, BChanMask, Lli, DataChan, Data) = CtrlBin,
		DataBin}}, State) ->
io:fwrite("Port=~w, MsgType=~.16x, L4Ref=~.16x, CallRef=~.16x, BChan=~w, Iface=~w, BChanMask=~.16x, Lli=~.16x, DataChan=~w~n", [Port, MsgType,"0x", L4Ref,"0x", CallRef,"0x", BChan, Iface, BChanMask,"0x", Lli,"0x", DataChan]),
	{Pid, _Time} = gb_trees:get({port, Port}, State),
	Pid ! {Port, {'L3L4m', CtrlBin, DataBin}},
	{noreply, State};

% an L3L4 SMI control message has arrived from the board
handle_info({Port, {'L3L4m', <<>>, DataBin}}, State) ->
	{Pid, _Time} = gb_trees:get({port, Port}, State),
	Pid ! {Port, {data, DataBin}},
	{noreply, State};

% a port has closed normally
handle_info({'EXIT', Port, normal}, State) ->
	NewState = clean_port(Port, State),
	{noreply, NewState};

% a port has closed abnormally
handle_info({'EXIT', Port, Reason}, State) ->
	error_logger:error_report([{port, Port}, {reason, Reason},
			"Port closed unexpectedly"]),
	NewState = clean_port(Port, State),
	{noreply, NewState};

handle_info(Unknown, State) ->
	error_logger:error_report([{message, Unknown}, {state, State},
			"Unknown message received"]),
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
do_ioctl(Request, Timeout) ->
	gen_server:call(netaccess_server, Request, Timeout).

do_call(Request) ->
	gen_server:call(netaccess_server, Request).

do_cast(Request) ->
	gen_server:cast(netaccess_server, Request).

% merges a list of option settings with the list of default values
mergeopts([], Merged) -> Merged;
mergeopts([{Option, Value}|T], Defaults) ->
	Merged = lists:keyreplace(Option, 1, Defaults, {Option, Value}),
	mergeopts(T, Merged).

clean_port(Port, State) when is_port(Port) ->
	I = gb_trees:iterator(State),
	clean_state(Port, State, I).

clean_state(Port, State, I) ->
	case gb_trees:next(I) of
		{{port, Port}, {_Pid, _Time}, S} ->
			NewState = gb_trees:delete({port, Port}, State),
			clean_state(Port, NewState, S);
		{{ref, Ref}, {Port, _From, _Time}, S} ->
			NewState = gb_trees:delete({ref, Ref}, State),
			clean_state(Port, NewState, S);
		{Key, _, S} ->
			clean_state(Port, State, S);
		none ->
			State
	end.
