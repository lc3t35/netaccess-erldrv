%%%---------------------------------------------------------------------
%%% @copyright Motivity Telecom Inc. 2004
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
%%% @doc A gen_fsm based behaviour for users of the netaccess device driver.
%%%
         
-module(netaccess_fsm).
-copyright('Copyright (c) 2004 Motivity Telecom Inc.').
-vsn('0.1').
-author('vances@motivity.ca').

-behaviour(gen_fsm).

% behaviour modules must export this function
-export([behaviour_info/1]).

% export the gen_fsm interface
-export([start/3, start/4, start_link/3, start_link/4,
		send_event/2, sync_send_event/2, sync_send_event/3,
		send_all_state_event/2, sync_send_all_state_event/2,
		sync_send_all_state_event/3, reply/2, send_event_after/2,
		start_timer/2, cancel_timer/1]).

% export the gen_fsm state handler call backs
-export([statename/2, statename/3]).

% export the gen_fsm common call backs
-export([init/1, handle_event/3, handle_sync_event/4,
		handle_info/3, terminate/3, code_change/4]).

%%
%% define what call backs users must export
%%
behaviour_info(callbacks) ->
	gen_fsm:behaviour_info(callbacks);
behaviour_info(Other) -> 
	gen_fsm:behaviour_info(Other).


%%----------------------------------------------------------------------
%%  The gen_fsm API functions
%%----------------------------------------------------------------------

start(Module, Args, Options) ->
	gen_fsm:start(?MODULE, [Module, Args], Options).

start(FsmRef, Module, Args, Options) ->
	gen_fsm:start(FsmRef, ?MODULE, [Module, Args], Options).

start_link(Module, Args, Options) ->
	gen_fsm:start_link(?MODULE, [Module, Args], Options).

start_link(FsmRef, Module, Args, Options) ->
	gen_fsm:start_link(FsmRef, ?MODULE, [Module, Args], Options).

send_event(FsmRef, Event) ->
	gen_fsm:send_event(FsmRef, Event).

sync_send_event(FsmRef, Event) ->
	gen_fsm:sync_send_event(FsmRef, Event).

sync_send_event(FsmRef, Event, Timeout) ->
	gen_fsm:sync_send_event(FsmRef, Event, Timeout).

send_all_state_event(FsmRef, Event) ->
	gen_fsm:send_all_state_event(FsmRef, Event).

sync_send_all_state_event(FsmRef, Event) ->
	gen_fsm:sync_send_all_state_event(FsmRef, Event).

sync_send_all_state_event(FsmRef, Event, Timeout) ->
	gen_fsm:sync_send_all_state_event(FsmRef, Event, Timeout).

reply(Caller, Reply) ->
	gen_fsm:reply(Caller, Reply).

send_event_after(Time, Event) ->
	gen_fsm:send_event_after(Time, Event).

start_timer(Time, Msg) ->
	gen_fsm:start_timer(Time, Msg).

cancel_timer(Ref) ->
	gen_fsm:cancel_timer(Ref).


%%----------------------------------------------------------------------
%%  The gen_fsm call backs
%%----------------------------------------------------------------------

init([Module, Args]) ->
	case Module:init(Args) of
		{ok, StateName, StateData} ->
			process_flag(trap_exit, true),
			{ok, statename, {Module, StateName, StateData}};
		{ok, StateName, StateData, Timeout} ->
			process_flag(trap_exit, true),
			{ok, statename, {Module, StateName, StateData}, Timeout};
		{stop, Reason} ->
			{stop, Reason};
		ignore ->
			ignore;
		Other ->
			Other
	end.
                

statename(Event, {Module, StateName, StateData}) ->
	case Module:StateName(Event, StateData) of
		{next_state, NextStateName, NewStateData} ->
			{next_state, statename, {Module, NextStateName, NewStateData}};
		{next_state, NextStateName, NewStateData, Timeout} ->
			{next_state, statename, {Module, NextStateName, NewStateData}, Timeout};
		{stop, Reason, NewStateData} ->
			{stop, Reason, {Module, StateName, NewStateData}};
		Other ->
			Other
	end.


statename(Event, From, {Module, StateName, StateData}) ->
	case Module:StateName(Event, From, StateData) of
		{reply, Reply, NextStateName, NewStateData} ->
			{reply, Reply, statename, {Module, NextStateName, NewStateData}};
		{reply, Reply, NextStateName, NewStateData, Timeout} ->
			{reply, Reply, statename, {Module, NextStateName, NewStateData}, Timeout};
		{next_state, NextStateName, NewStateData} ->
			{next_state, statename, {Module, NextStateName, NewStateData}};
		{next_state, NextStateName, NewStateData, Timeout} ->
			{next_state, statename, {Module, NextStateName, NewStateData}, Timeout};
		{stop, Reason, Reply, NewStateData} ->
			{stop, Reason, Reply, {Module, StateName, NewStateData}};
		{stop, Reason, NewStateData} ->
			{stop, Reason, {Module, StateName, NewStateData}};
		Other ->
			Other
	end.


handle_event(Event, statename, {Module, StateName, StateData}) ->
	case Module:StateName(Event, StateData) of
		{next_state, NextStateName, NewStateData} ->
			{next_state, statename, {Module, NextStateName, NewStateData}};
		{next_state, NextStateName, NewStateData, Timeout} ->
			{next_state, statename, {Module, NextStateName, NewStateData}, Timeout};
		{stop, Reason, NewStateData} ->
			{stop, Reason, {Module, StateName, NewStateData}};
		Other ->
			Other
	end.


handle_sync_event(Event, From, statename, {Module, StateName, StateData}) ->
	case Module:handle_sync_event(Event, From, StateName, StateData) of
		{reply, Reply, NextStateName, NewStateData} ->
			{reply, Reply, statename, {Module, NextStateName, NewStateData}};
		{reply, Reply, NextStateName, NewStateData, Timeout} ->
			{reply, Reply, statename, {Module, NextStateName, NewStateData}, Timeout};
		{next_state, NextStateName, NewStateData} ->
			{next_state, statename, {Module, NextStateName, NewStateData}};
		{next_state, NextStateName, NewStateData, Timeout} ->
			{next_state, statename, {Module, NextStateName, NewStateData}, Timeout};
		{stop, Reason, Reply, NewStateData} ->
			{stop, Reason, Reply, {Module, StateName, NewStateData}};
		{stop, Reason, NewStateData} ->
			{stop, Reason, {Module, StateName, NewStateData}};
		Other ->
			Other
	end.


%% an L3L4 SMI control message arrived from the driver
handle_info({Port, {'L3L4m', CtrlBin, DataBin}}, statename, {Module, StateName, StateData}) 
			when is_binary(CtrlBin), size(CtrlBin) > 0 ->
	L3L4_rec = iisdn:l3_to_l4(CtrlBin),
	case Module:StateName({Port, L3L4_rec}, StateData) of
		{next_state, NextStateName, NewStateData} ->
			{next_state, statename, {Module, NextStateName, NewStateData}};
		{next_state, NextStateName, NewStateData, Timeout} ->
			{next_state, statename, {Module, NextStateName, NewStateData}, Timeout};
		{stop, Reason, NewStateData} ->
			{stop, Reason, {Module, StateName, NewStateData}};
		Other ->
			Other
	end;
%% an L3L4 data message arrived from the driver
handle_info({Port, {'L3L4m', _CtrlBin, DataBin}}, statename, {Module, StateName, StateData}) 
			when is_binary(DataBin), size(DataBin) > 0 ->
	case Module:StateName({Port, DataBin}, StateData) of
		{next_state, NextStateName, NewStateData} ->
			{next_state, statename, {Module, NextStateName, NewStateData}};
		{next_state, NextStateName, NewStateData, Timeout} ->
			{next_state, statename, {Module, NextStateName, NewStateData}, Timeout};
		{stop, Reason, NewStateData} ->
			{stop, Reason, {Module, StateName, NewStateData}};
		Other ->
			Other
	end;
handle_info(Event, statename, {Module, StateName, StateData}) ->
	case Module:handle_info(Event, StateName, StateData) of
		{next_state, StateName, StateData} ->
			{next_state, statename, {Module, StateName, StateData}};
		{next_state, NextStateName, NewStateData, Timeout} ->
			{next_state, statename, {Module, NextStateName, NewStateData}, Timeout};
		{stop, Reason, NewStateData} ->
			{stop, Reason, {Module, StateName, NewStateData}};
		Other ->
			Other
	end.


terminate(Reason, statename, {Module, StateName, StateData}) ->
	Module:terminate(Reason, StateName, StateData).


code_change(OldVersion, statename, {Module, StateName, StateData}, Extra) ->
	case Module:code_change(OldVersion, StateName, StateData, Extra) of
		{ok, NextStateName, NewStateData} ->
			{ok, statename, {Module, NextStateName, NewStateData}};
		Other ->
			Other
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

