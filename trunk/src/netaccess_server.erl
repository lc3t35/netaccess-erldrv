%%%---------------------------------------------------------------------
%%% @copyright Motivity Telecom Inc. 2001-2004
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
%%% @doc The Erlang side of the netaccess device driver.
%%%
         
-module(netaccess_server).
-copyright('Copyright (c) 2001-2004 Motivity Telecom Inc.').
-vsn('0.1').
-author('vances@motivity.ca').

-behaviour(gen_server).

-include("pridrv.hrl").
-include("iisdn.hrl").

%% call backs needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% This module implements the Erlang side of the netaccess device driver
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%----------------------------------------------------------------------
%%  The gen_server call backs
%%----------------------------------------------------------------------

%% initialize the netaccess server
init([])  ->
	init([?DEFAULT_BOARDNAME]);
init([BoardName]) ->
	init([BoardName, 0]);
init([BoardName, BoardNumber]) when is_list(BoardName),
		is_integer(BoardNumber) ->
	PoolSize = erlang:system_info(thread_pool_size),
	init_threads(PoolSize, [BoardName, BoardNumber]).
init_threads(0, [_BoardName, _BoardNumber]) ->
	{stop, no_threads};
init_threads(_, [BoardName, BoardNumber]) ->
	init_start([BoardName, BoardNumber]).

init_start([BoardName, BoardNumber]) ->
	process_flag(trap_exit, true),
	erl_ddll:start(),
	% load the dynamically linked device driver
	PrivDir = code:priv_dir(netaccess),
	LibDir = filename:join([PrivDir, "lib"]),
	init_driver(erl_ddll:load_driver(LibDir, netaccess_drv),
			BoardName, BoardNumber).

init_driver({error, ErrorDescriptor}, _BoardName, _BoardNumber) ->
	ErrorString = erl_ddll:format_error(ErrorDescriptor),
	error_logger:error_msg(ErrorString),
	{stop, ErrorDescriptor};
init_driver(ok, BoardName, BoardNumber) ->
	Command = list_to_atom("netaccess_drv " ++ BoardName),
	Result = (catch erlang:open_port({spawn, Command}, [binary])),
	init_port(Result, BoardName, BoardNumber).

init_port(Port, BoardName, BoardNumber) when is_port(Port) ->
	Result = blocking_ioctl(Port, ?SELECT_BOARD, BoardNumber),
	init_select(Result, Port, {BoardName, BoardNumber});
init_port({'EXIT', Reason}, _BoardName, _BoardNumber) ->
	{stop, Reason}.

init_select(ok, Port, Board) ->
	Result = blocking_ioctl(Port, ?ENABLE_MANAGEMENT_CHAN, 0),
	init_enable(Result, Port, Board);
init_select(Reason, Port, _Board) ->
	erlang:port_close(Port),
	{stop, Reason}.

init_enable(ok, Port, Board) ->
	NewState = {Port, Board, gb_trees:empty()},
	{ok, NewState};
init_enable(Reason, Port, _Board) ->
	erlang:port_close(Port),
	{stop, Reason}.

	
%% open a port to the netaccess board
handle_call(open, {Pid, _Tag} = From, {ManagementPort, {BoardName, BoardNumber} = Board,
		StateData} = State) when node(Pid) == node() ->
	Command = list_to_atom("netaccess_drv " ++ BoardName),
	case catch erlang:open_port({spawn, Command}, [binary]) of
		Channel when is_port(Channel) -> 
			case catch erlang:port_call(Channel, ?SELECT_BOARD, BoardNumber) of
				Ref when is_integer(Ref) ->
					NewStateData = gb_trees:insert({ref, Ref}, {From, Channel, now()}, StateData), 
					{noreply, {ManagementPort, Board, NewStateData}};
				{'EXIT', Reason} ->
					erlang:port_close(Channel),
					exit(Pid, Reason),
					{noreply, State}
			end;
		{'EXIT', Reason} ->
			exit(Pid, Reason),
			{noreply, State}
	end;
%% since the port gets linked to the owner it must be local
handle_call(open, {Pid, _Tag}, State) when node(Pid) /= node() ->
	exit(Pid, badarg),
	{noreply, State};

%% perform an ioctl on an open channel to a netaccess board
handle_call({ioctl, Operation, Data}, From, {Port, Board, StateData} = State) ->
	case catch erlang:port_call(Port, Operation, Data) of
		Ref when is_integer(Ref) ->
			NewStateData = gb_trees:insert({ref, Ref},
					{From, now()}, StateData),
			{noreply, {Port, Board, NewStateData}};
		{'EXIT', Reason} ->
			{Pid, _Tag} = From,
			exit(Pid, Reason),
			{noreply, State}
	end;

%% discriminate synchronous from asynchronous L4L3m requests
handle_call({'L4L3m', L4L3_Rec, _Data}, From, State) when
		is_record(L4L3_Rec, l4_to_l3), L4L3_Rec#l4_to_l3.msgtype == ?L4L3mREQ_BOARD_ID;
		is_record(L4L3_Rec, l4_to_l3), L4L3_Rec#l4_to_l3.msgtype == ?L4L3mREQ_HW_STATUS;
		is_record(L4L3_Rec, l4_to_l3), L4L3_Rec#l4_to_l3.msgtype == ?L4L3mREQ_TSI_STATUS ->
	% we need to set the lapdid value to one which is not in use otherwise
	% the responses will be delivered on the stream which has specified that
	% lapdid in an L4L3mENABLE_PROTOCOL messages!
	handle_call_sync(L4L3_Rec#l4_to_l3{lapdid = 16#ff}, From, State);
handle_call({'L4L3m', L4L3_Rec, _Data}, From, State) ->
	handle_call_async(L4L3_Rec#l4_to_l3{lapdid = 16#ff}, From, State);

%% ignore unknown requests
handle_call(_, _, State) ->
	{noreply, State}.


%% Synchronous requests
%%
%% try to encode the record as a binary
handle_call_sync(L4L3_Rec, From, State)  when is_record(L4L3_Rec, l4_to_l3) ->
	Result = (catch iisdn:l4_to_l3(L4L3_Rec)),
	handle_call_sync(L4L3_Rec#l4_to_l3.msgtype, Result, From, State).
%% try to insert the request
handle_call_sync(MsgType, L4L3_Bin, From, {_Port, _Board, StateData} = State) when is_binary(L4L3_Bin) ->
	Timeout = timeout(2000),
	Result = (catch gb_trees:insert(MsgType, {From, Timeout, []}, StateData)),
	handle_call_sync(MsgType, L4L3_Bin, From, Result, State);
%% failed to encode record
handle_call_sync(_MsgType, {'EXIT', _Reason}, {Pid, _Tag}, State) ->
	exit(Pid, badarg),
	{noreply, State}.
%% we're holding a request already, check if it's stale
handle_call_sync(MsgType, L4L3_Bin, From, {'EXIT', _Reason}, {Port, Board, StateData} = State) ->
	Now = now(),
	case gb_trees:lookup(MsgType, StateData) of
		{value, {{Pid, _Tag}, Time, _Acc}} when Time < Now ->
			% request in progress, raise exception
			exit(Pid, ebusy),
			{noreply, State};
		{value, {From, Time, _Acc}} when Time > Now ->
			% overwrite stale request
			Timeout = timeout(2000),
			NewStateData = gb_trees:update(MsgType, {From, Timeout, []}, StateData),
			% send to port
			erlang:port_call(Port, ?L4L3m, L4L3_Bin),
			error_logger:error_report(["Stale request", {msgtyp, MsgType}, {tree, StateData}]),
			{noreply, {Port, Board, NewStateData}}
	end;
%% insertion succeeded, send to port
handle_call_sync(_MsgType, L4L3_Bin, _From, NewStateData, {Port, Board, _StateData}) ->
	erlang:port_call(Port, ?L4L3m, L4L3_Bin),
	{noreply, {Port, Board, NewStateData}}.
	
%% Asynchronous requests
%%
%% try to encode the record as a binary
handle_call_async(L4L3_Rec, From, State)  when is_record(L4L3_Rec, l4_to_l3) ->
	Result = (catch iisdn:l4_to_l3(L4L3_Rec)),
	handle_call_async(L4L3_Rec#l4_to_l3.msgtype, Result, From, State).
%% send to port
handle_call_async(_MsgType, L4L3_Bin, _From, {Port, _Board, _StateData} = State) when is_binary(L4L3_Bin) ->
	erlang:port_call(Port, ?L4L3m, L4L3_Bin),
	{reply, ok, State};
%% failed to encode record
handle_call_async(_MsgType, {'EXIT', _Reason}, {Pid, _Tag}, State) ->
	exit(Pid, badarg),
	{noreply, State}.


%% shutdown the netaccess server
handle_cast(stop, {ManagementPort, _Board, _StateData} = State) ->
	catch port_close(ManagementPort),
	{stop, shutdown, State};

handle_cast({ioctl, Operation, Data, Port}, State) ->
	catch erlang:port_call(Port, Operation, Data),
	{noreply, State};
handle_cast(_, State) ->
	{noreply, State}.


%% an asynch task has completed
handle_info({Channel, {ref, Ref}, Result}, {ManagementPort, Board, StateData} = State) ->
	case gb_trees:lookup({ref, Ref}, StateData) of
		{value, {From, _Time}} ->
			% a generic ioctl operation
			NewStateData = gb_trees:delete({ref, Ref}, StateData),
			gen_server:reply(From, Result),
			{noreply, {ManagementPort, Board, NewStateData}};
		{value, {{Pid, _Tag} = From, Channel, _Time}} ->
			% a select ioctl for an open
			NewStateData = gb_trees:delete({ref, Ref}, StateData),
			port_connect(Channel, Pid),
			unlink(Channel),
			gen_server:reply(From, Channel),
			{noreply, {ManagementPort, Board, NewStateData}};
		none ->
			error_logger:error_report([{server, self()}, {ref, Ref},
					{port, Channel}, {result, Result},
					"Misdirected reply from netaccess driver"]),
			{noreply, State}
	end;
% an L3L4 SMI message binary arrived from the board
handle_info({Port, {'L3L4m', CtrlBin, DataBin}}, {Port, _Board, _StateData} = State) 
			when is_binary(CtrlBin), size(CtrlBin) > 0 ->
	case catch iisdn:l3_to_l4(CtrlBin) of
		L3L4_rec when is_record(L3L4_rec, l3_to_l4) ->
			handle_info({Port, {'L3L4m', L3L4_rec, DataBin}}, State);
		{'EXIT', Reason} ->
			error_logger:error_report(["Netaccess server received corrupt L3L4m",
					Reason, {port, Port}, {control, CtrlBin}, {data, DataBin}]),
			{noreply, State}
	end;
% an L3L4mBOARD_ID message
handle_info({Port, {'L3L4m', L3L4_rec, _DataBin}}, {Port, Board, StateData} = State)
		when is_record(L3L4_rec, l3_to_l4),
		L3L4_rec#l3_to_l4.msgtype == ?L3L4mBOARD_ID ->
	case gb_trees:lookup(?L4L3mREQ_BOARD_ID, StateData) of
		{value, {From, _Time, _Acc}} ->
			NewStateData = gb_trees:delete(?L4L3mREQ_BOARD_ID, StateData),
			case catch iisdn:board_id(L3L4_rec#l3_to_l4.data) of
				BoardId when is_record(BoardId, board_id) ->
					gen_server:reply(From, BoardId),
					{noreply, {Port, Board, NewStateData}};
				{'EXIT', _Reason} -> 
					error_logger:info_report(["Netaccess server received unhandled "
							"L3L4mBOARD_ID", L3L4_rec]),
					{noreply, {Port, Board, NewStateData}}
			end;
		none ->
			error_logger:info_report(["Netaccess server received unhandled "
					"L3L4mBOARD_ID", L3L4_rec]),
			{noreply, State}
	end;
% an L3L4mHARDWARE_STATUS message
handle_info({Port, {'L3L4m', L3L4_rec, _DataBin}}, {Port, Board, StateData} = State)
		when is_record(L3L4_rec, l3_to_l4),
		L3L4_rec#l3_to_l4.msgtype == ?L3L4mHARDWARE_STATUS ->
	case gb_trees:lookup(?L4L3mREQ_HW_STATUS, StateData) of
		{value, {From, _Time, _Acc}} ->
			NewStateData = gb_trees:delete(?L4L3mREQ_HW_STATUS, StateData),
			HardwareData = iisdn:hardware_data(L3L4_rec#l3_to_l4.data),
			gen_server:reply(From, HardwareData),
			{noreply, {Port, Board, NewStateData}};
		none ->
			error_logger:info_report(["Netaccess server received unhandled "
					"L3L4mHARDWARE_STATUS", L3L4_rec]),
			{noreply, State}
	end;
% an L3L4mTSI_STATUS message 
handle_info({Port, {'L3L4m', L3L4_rec, DataBin}}, {Port, Board, StateData} = State)
		when is_record(L3L4_rec, l3_to_l4),
		L3L4_rec#l3_to_l4.msgtype == ?L3L4mTSI_STATUS ->
	case gb_trees:lookup(?L4L3mREQ_TSI_STATUS, StateData) of
		{value, {From, _Time, Acc}} ->
			case catch iisdn:tsi_data(L3L4_rec#l3_to_l4.data) of
				% there are more on the way
				TsiDataRec when is_record(TsiDataRec, tsi_data),
						TsiDataRec#tsi_data.last == 0 -> 
					NewStateData = gb_trees:update(?L4L3mREQ_TSI_STATUS,
							{From, _Time, Acc ++ [TsiDataRec]}, StateData),
					{noreply, {Port, Board, NewStateData}};
				% this is the last one
				TsiDataRec when is_record(TsiDataRec, tsi_data) ->
					NewStateData = gb_trees:delete(?L4L3mREQ_TSI_STATUS, StateData),
					gen_server:reply(From, {ok, Acc ++ [TsiDataRec]}),
					{noreply, {Port, Board, NewStateData}};
				{'EXIT', Reason} ->
					NewStateData = gb_trees:delete(?L4L3mREQ_TSI_STATUS, StateData),
					error_logger:error_report(["Netaccess server received corrupt L3L4mTSI_STATUS",
							Reason, {port, Port}, {control, L3L4_rec}, {data, DataBin}]),
					{noreply, {Port, Board, NewStateData}}
			end;
		none ->
			error_logger:info_report(["Netaccess server received unhandled "
					"L3L4mTSI_STATUS", L3L4_rec]),
			{noreply, State}
	end;
% an L3L4mLINE_STATUS message
handle_info({Port, {'L3L4m', L3L4_rec, _DataBin}}, {Port, _Board, _StateData} = State)
		when is_record(L3L4_rec, l3_to_l4),
		L3L4_rec#l3_to_l4.msgtype == ?L3L4mLINE_STATUS ->
	LineStatus = (catch iisdn:line_status(L3L4_rec#l3_to_l4.data)),
	error_logger:info_report(["Netaccess server received L3L4mLINE_STATUS", 
			{port, Port}, {lapdid, L3L4_rec#l3_to_l4.lapdid}, {line_status, LineStatus}]),
	{noreply, State};
% an L3L4mERROR message
handle_info({Port, {'L3L4m', L3L4_rec, _DataBin}}, {Port, _Board, _StateData} = State)
		when is_record(L3L4_rec, l3_to_l4),
		L3L4_rec#l3_to_l4.msgtype == ?L3L4mERROR ->
	ErrorCode = (catch iisdn:error_code(L3L4_rec#l3_to_l4.data)),
	error_logger:info_report(["Netaccess server received L3L4mERROR", 
			{port, Port}, {lapdid, L3L4_rec#l3_to_l4.lapdid},
			{error, ErrorCode#error_code.error_code},
			{offending_message, ErrorCode#error_code.offending_message}]),
	{noreply, State};
% an L3L4 SMI message arrived from the board
handle_info({Port, {'L3L4m', L3L4, _DataBin}}, {Port, _Board, _StateData} = State) ->
	error_logger:info_report(["Netaccess server received unhandled L3L4m", L3L4]),
	{noreply, State};
% our management port has closed
handle_info({'EXIT', Port, Reason}, {Port, _Board, _StateData} = State) ->
	{stop, Reason, State};
% a port we were opening closed before we transfered ownership
handle_info({'EXIT', Port, _Reason}, State) when is_port(Port) ->
	{noreply, State};
% someone wants us to shutdown and cleanup
handle_info({'EXIT', Pid, shutdown}, State) when is_pid(Pid) ->
	{stop, shutdown, State};
% an abnormal exit condition
handle_info({'EXIT', Pid, Reason}, State) when is_pid(Pid) ->
	{stop, Reason, State};
% unknown message
handle_info(Unknown, State) ->
	error_logger:error_report([{server, self()}, {message, Unknown},
			"Netaccess server received unknown message"]),
	{noreply, State}.

%% exit gracefully
terminate(_Reason, _State) ->
	catch erl_ddll:unload_driver(netaccess_drv).

%% new code version has been loaded
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

blocking_ioctl(Port, Operation, Data) ->
	case catch erlang:port_call(Port, Operation, Data) of
		Ref when is_integer(Ref)  ->
			receive
				{Port, {ref, Ref}, Result} ->
					Result
			after 2000 ->
				timeout
			end;
		{'EXIT', Reason} ->
			Reason
	end.


%% add Us microseconds to a now() value
timeout(Ms) when is_integer(Ms) ->
	Us = Ms * 1000,
	{Mi, Si, Ui} = now(),
	Uo = (Ui + Us) rem 1000000,
	So = (Si + ((Ui + Us) div 1000000)) ,
	{Mi + (So div 1000000), So rem 1000000, Uo}.
	
