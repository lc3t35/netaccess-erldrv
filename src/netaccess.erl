%%%---------------------------------------------------------------------
%%% @copyright Motivity Telecom Inc. 2001-2004
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
%%% @doc Main <acronym title="Application Programming Interface>API
%%%			</acronym> of the Netaccess application.
%%%	<p>This module provides the main application programming interface
%%%	for the Netaccess application.  The application includes a
%%%	dynamically linked in device driver to the Netaccess series boards
%%%	from Brooktrout Technology.  This module is an Erlang binding to
%%%	the Instant ISDN&trade; Simple Message Interface (SMI) API.</p>
%%% @end
         
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
-export([enable_protocol/3, enable_protocol/4, enable_protocol/7]).

-include("pridrv.hrl").
-include("iisdn.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% This module implements the Erlang side of the netaccess device driver
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%----------------------------------------------------------------------
%%  The gen_server call backs
%%----------------------------------------------------------------------

%% @spec () -> {ok, pid()} | {error, Reason::term()}
%%
%% @doc Start the netacccess server.
%%
%% @see gen_server:start/4
%%
start() ->
	gen_server:start({local, netaccess_server}, ?MODULE, [], []).
	
%% @spec () -> {ok, pid()} | {error, Reason::term()}
%%
%% @doc Start the netacccess server and link to the calling process.
%%
%% @see gen_server:start_link/4
%%
start_link() ->
	gen_server:start_link({local, netaccess_server}, ?MODULE, [], []).


%% @spec () -> ok
%%
%% @doc Stop the netaccess server.
%%
stop() ->
	do_call(stop).


%% @spec () -> {ok, Channel::port()}
%%
%% @doc Open a channel on the netaccess board.
%%
%% @see erlang:open_port/2
%%
open() ->
	do_call({open, "netaccess_drv"}).

%% @spec (Board::list() | Board::atom()) -> {ok, Channel::port()}
%%
%% @doc Open a channel on a specified netaccess board.
%%
%% @see erlang:open_port/2
%%
open(Board) when list(Board) ->
	do_call({open, "netaccess_drv " ++ Board});
open(Board) when atom(Board) ->
	do_call({open, "netaccess_drv " ++ atom_to_list(Board)}).


%% @spec (Channel::port()) -> true
%%
%% @doc Close a channel on a netaccess board.
%%
%% @see erlang:port_close/1
%%
close(Port) ->
	do_call({close, Port}).


%% @spec (Channel::port(), Board::integer()) -> ok
%%
%% @doc Select a netaccess board for the channel.
%%
select_board(Port, Board) when integer(Board) ->
	do_ioctl({ioctl, ?SELECT_BOARD, Board, Port}).


%% @spec (Channel::port()) -> ok
%%
%% @doc Enable a management channel on an open netaccess board.
%%
enable_management_chan(Port) ->
	do_ioctl({ioctl, ?ENABLE_MANAGEMENT_CHAN, [], Port}).


%% @spec (Channel::port(), BootFile) -> ok | {error, Reason:term()}
%%		BootFile = binary() | string() | atom()
%%
%% @doc Boot an open netaccess board.
%%		<p>BootFile may be either the boot code itself in a binary
%%		or the file name of the boot file.</p>
%%
%%		<p>If the kernel is 64 bit the Erlang emulator as well
%%		as the netaccess driver must be compiled as 64 bit for
%%		this to work.  Otherwise it will return "badarg".</p>
%%
%% @see file:read_file/1
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


%% @spec (Channel::port()) -> ok
%%
%% @doc Perform a reset on an open netaccess board.
%%
reset_board(Port) ->
	do_ioctl({ioctl, ?RESET_BOARD, [], Port}).


%% @spec (Channel:port()) -> returns {ok, Version::string()} or {error, Reason::term()}
%%
%% @doc Get software version string from an open netaccess board.
%%
get_version(Port) ->
	do_ioctl({ioctl, ?GET_VERSION, [], Port}).


%% @spec (Channel:port()) -> returns {ok, driver_info()} or {error, Reason::term()}
%%
%% @type driver_info(). A record which includes the following fields:
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
%% @doc Get driver information from an open netaccess board.
%%
%%
get_driver_info(Port) ->
	case do_ioctl({ioctl, ?GET_DRIVER_INFO, [], Port}) of
		{ok, DriverInfo} ->
			{ok, pridrv:driver_info(DriverInfo)};
		Return -> Return
	end.
	

%%
%% @doc Set hardware settings on board such as clocking, framing, etc.
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
			lists:duplicate(?IISDN_MAX_LINES, CsuFlag));

% may be called with only one list of line settings in which
% case we will use these settings for all spans
set_hardware(Port, HardwareSettings, LineSettings, CsuFlags) 
		when list(LineSettings), tuple(hd(LineSettings)) ->
	set_hardware(Port, HardwareSettings,
			lists:duplicate(?IISDN_MAX_LINES, LineSettings), CsuFlags);

% may be called with an unordered subset of settings
set_hardware(Port, HardwareSettings, LineSettings, CsuFlags) 
		when list(LineSettings), length(LineSettings) == ?IISDN_MAX_LINES,
		list(CsuFlags), length(CsuFlags) == ?IISDN_MAX_LINES ->
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


%% @spec (Channel::port()) -> {ok, hardware_data()}
%%
%% @type hardware_data().  A record which includes the following fields:
%% 	<dl>
%% 		<dt>clocking</dt> <dd><code>integer()</code></dd>
%% 		<dt>clocking2</dt> <dd><code>integer()</code></dd>
%% 		<dt>enable_clocking2</dt> <dd><code>integer()</code></dd>
%% 		<dt>netref_clocking</dt> <dd><code>integer()</code></dd>
%% 		<dt>netref_rate</dt> <dd><code>integer()</code></dd>
%% 		<dt>ctbus_mode</dt> <dd><code>integer()</code></dd>
%% 		<dt>force_framer_init</dt> <dd><code>integer()</code></dd>
%% 		<dt>tdm_rate</dt> <dd><code>integer()</code></dd>
%% 		<dt>enable_8370_rliu_monitor</dt> <dd><code>integer()</code></dd>
%% 		<dt>dbcount</dt> <dd><code>integer()</code></dd>
%% 		<dt>enable_t810x_snap_mode</dt> <dd><code>integer()</code></dd>
%% 		<dt>clk_status</dt> <dd><code>integer()</code></dd>
%% 		<dt>line_data</dt> <dd><code>[line_data()]</code></dd>
%% 		<dt>csu</dt> <dd><code>[boolean()]</code></dd>
%% 	</dl>
%%
%% @type line_data(). A record which includes the following fields:
%% 	<dl>
%% 		<dt>framing</dt> <dd><code>integer()</code></dd>
%% 		<dt>line_code</dt> <dd><code>integer()</code></dd>
%% 		<dt>pm_mode</dt> <dd><code>integer()</code></dd>
%% 		<dt>line_length</dt> <dd><code>integer()</code></dd>
%% 		<dt>term</dt> <dd><code>integer()</code></dd>
%% 		<dt>line_type</dt> <dd><code>integer()</code></dd>
%% 		<dt>integrate_alarms</dt> <dd><code>integer()</code></dd>
%% 		<dt>filter_unsolicited</dt> <dd><code>integer()</code></dd>
%% 		<dt>filter_yellow</dt> <dd><code>integer()</code></dd>
%% 		<dt>bri_l1mode</dt> <dd><code>integer()</code></dd>
%% 		<dt>briL1_cmd</dt> <dd><code>integer()</code></dd>
%% 		<dt>bri_loop</dt> <dd><code>integer()</code></dd>
%% 		<dt>briL1_T3</dt> <dd><code>integer()</code></dd>
%% 		<dt>briL1_T4</dt> <dd><code>integer()</code></dd>
%% 	</dl>
%%
%% @doc Query the hardware setup.
%%
req_hw_status(Port) ->
	L4L3_Bin = iisdn:'L4_to_L3_struct'(#'L4_to_L3_struct'{msgtype = ?L4L3mREQ_HW_STATUS}),
	port_command(Port, L4L3_Bin),
	receive 
		{Port, {'L3L4m', <<_LapdId:?IISDNu8bit, ?L3L4mHARDWARE_STATUS,
				_L4Ref:?IISDNu16bit, _CallRef:?IISDNu16bit, _BChan:?IISDNu8bit,
				_Iface:?IISDNu8bit, _BChanMask:?IISDNu32bit, _Lli:?IISDNu16bit,
				_DataChan:?IISDNu16bit, HardwareData/binary>>}} ->
			iisdn:'IISDN_HARDWARE_DATA'(HardwareData);
		{Port, {error, Reason}} -> {error, Reason}
	after
		1000 -> {error, timeout}
	end.
	
	
%%
%% create timeslot mappings
%%
set_tsi(Port, Granularity, TsiMapTerms) ->
	set_tsi(Port, length(TsiMapTerms), Granularity, TsiMapTerms, <<>>).
set_tsi(Port, NumMappings, Granularity, [], TsiMapBins) ->
	L4L3_Bin = ?L4L3_Mask(0, ?L4L3mSET_TSI, 16#FFFF, 0, 0),
	Last = 0,
	L4_to_L3_struct = concat_binary([L4L3_Bin, ?IISDN_TSI_DATA]),
	port_command(Port, L4_to_L3_struct),
	receive 
		{Port, {error, Reason}} -> {error, Reason};
		{Port, {?L3L4_Mask(_LapdId, ?L3L4mERROR, _L4Ref, _CallRef,
				_BChan, _IFace, _BChanMask, _Lli, _DataChan, Rest), _DataBin}} ->
			<<Error:?IISDNu8bit, _/binary>> = Rest,
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
			?IISDN_TSI_DATA = Rest,
			{ok, {num_mappings, NumMappings},
					{granularity, Granularity}, {last, Last},
					req_tsi_status(TsiMapBins, NumMappings, [])};
		{Port, {error, Reason}} -> {error, Reason}
	after
		1000 -> {error, timeout}
	end.
req_tsi_status(TsiMapBins, 0, TsiTerms) -> TsiTerms;
req_tsi_status(TsiMapBins, NumMaps, TsiTerms) ->
	<<?TsiMapMask, Rest/binary>> = split_binary(TsiMapBins, 4),
	req_tsi_status(Rest, NumMaps - 1, TsiTerms ++ ?TsiMapTerms).
	

%%
%% enable_protocol(Port, Lapdid, Command)
%% enable_protocol(Port, Lapdid, Command, CommandParameter)
%% enable_protocol(Port, Lapdid, Command, CommandParameter,
%%                 Level1, Level2, Level3) ->
%% 
%%    Lapdid           = int()
%%    Command          = int()
%%    CommandParameter = int()
%%    Level1           = record() of type pri_level1_config
%%    Level2           = record() of type pri_level2_config
%%    Level3           = record() of type pri_level3_config
%%
%% Use the naii library to decode/encode these records and the
%% binaries received/sent to the boards.
%%
enable_protocol(Port, Lapdid, Command) ->
	enable_protocol(Port, Lapdid, Command, 0).
enable_protocol(Port, Lapdid, Command, CommandParameter) ->
	enable_protocol(Port, Lapdid, Command, CommandParameter,
			<<>>, <<>>, <<>>).
enable_protocol(Port, Lapdid, Command, CommandParameter,
		Level1, Level2, Level3) ->
	EP = #'IISDN_ENA_PROTO_DATA'{
			command=Command, 
			command_parameter=CommandParameter, 
			level1=Level1, level2=Level2, level3=Level3},
	L4L3m = #'L4_to_L3_struct'{
			lapdid = Lapdid,
			msgtype = ?L4L3mENABLE_PROTOCOL,
			data = naii:'IISDN_ENA_PROTO_DATA'(EP)},
	port_command(Port, naii:'L4_to_L3_struct'(L4L3m)).
	
	

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
