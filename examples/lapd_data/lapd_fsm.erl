%%%---------------------------------------------------------------------
%%% @copyright Motivity Telecom Inc. 2004
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
%%% @doc An finite state machine which starts a LAPD protocol and sends
%%% 		and receives IFRAMES, checking their integrity on reception.
%%%
         
-module(lapd_fsm).
-behaviour(netaccess_fsm).
-include("iisdn.hrl").

-export([init/1, terminate/3]).
-export([establishing/2, established/2, not_established/2]).
-export([handle_event/3, handle_info/3, handle_sync_event/4, code_change/4]).

-define(MAXIFRAMESZ, 260).

init([ServerRef, LapdId, Timing]) ->
	% open a lapid on the board
	case netaccess:open(ServerRef) of
		Channel when is_port(Channel) ->
			init_protocol(ServerRef, Channel, LapdId, Timing);
		{'EXIT', Reason} ->
			{stop, Reason}
	end.

init_protocol(ServerRef, Channel, LapdId, Timing) ->
 	L1 = #level1{l1_mode = ?IISDNl1modHDLC},
	L2Parms = #l2_lap_params{mode = ?IISDNl2modLAP_D,
			dce_dte = ?IISDNdirSYMMETRIC},
	D = #data_interface{enable = 1},
 	L2 = #level2{par = L2Parms, data_interface = D},
	ProtoData = #ena_proto_data{level1 = L1, level2 = L2},
	% send an L4L3mENABLE_PROTOCOL to start LAPD 
	netaccess:enable_protocol(Channel, LapdId, ProtoData),
	{ok, establishing, {ServerRef, Channel, LapdId, Timing}}.


%% waiting to establish multiframe state
establishing({Channel, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mPROTOCOL_STATUS ->
	P = iisdn:protocol_stat(L3L4m#l3_to_l4.data),
	case 	P#protocol_stat.status of
		?IISDNdsESTABLISHING ->
			error_logger:info_msg("LAPDID ~w ESTABLISHING~n",
					[element(3, StateData)]),
			{next_state, establishing, StateData};
		?IISDNdsESTABLISHED ->
			error_logger:info_msg("LAPDID ~w ESTABLISHED~n",
					[element(3, StateData)]),
			{Delay, _} = random:uniform_s(element(4, StateData), now()),
			gen_fsm:start_timer(Delay, timeout),
			{next_state, established, StateData}
	end;
establishing({port, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mERROR ->
	Reason = iisdn:error_code(L3L4m#l3_to_l4.data),
	{stop, Reason, StateData}.


%% multiframe state established
established({Channel, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mPROTOCOL_STATUS ->
	P = iisdn:protocol_stat(L3L4m#l3_to_l4.data),
	case 	P#protocol_stat.status of
		?IISDNdsNOT_ESTABLISHED ->
			error_logger:info_msg("LAPDID ~w NOT ESTABLISHED~n",
					[element(3, StateData)]),
			{next_state, not_established, StateData}
	end;
%% receive an IFRAME
established({Channel, <<Hash:8/unit:8, Data/binary>>}, StateData) ->
	case erlang:phash2(Data) of
		Hash -> 
			{next_state, established, StateData};
		_ ->
			{stop, bad_hash, StateData}
	end;	
established({timeout, Ref, timeout}, {ServerRef, Channel, LapdId, Timeout} = StateData) ->
	% send an IFRAME
	netaccess:send(Channel, iframe()),
	gen_fsm:start_timer(Timeout, timeout),
	{next_state, established, StateData};
established({Channel, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mERROR ->
	Reason = iisdn:error_code(L3L4m#l3_to_l4.data),
	{stop, Reason, StateData}.

%% multiframe not established
not_established({Channel, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mPROTOCOL_STATUS ->
	P = iisdn:protocol_stat(L3L4m#l3_to_l4.data),
	case 	P#protocol_stat.status of
		?IISDNdsESTABLISHING ->
			error_logger:info_msg("LAPDID ~w ESTABLISHING~n",
					[element(3, StateData)]),
			{next_state, establishing, StateData}
	end;
not_established({Channel, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mERROR ->
	Reason = iisdn:error_code(L3L4m#l3_to_l4.data),
	{stop, Reason, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.
	
handle_sync_event(_Event, _From, _StateName, StateData) ->
	StateData.
	
handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, {_ServerRef, Channel, _LapdId, _Timeout}) ->
	netaccess:close(Channel).

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%---------------------------------------------------------------------
%% internal functions
%%---------------------------------------------------------------------

%% create an IFRAME with an 8 octet header
%% containg a hash of the remaining payload
iframe() ->
	R = randb(),
	H = erlang:phash2(R),
	<<H:8/unit:8, R/binary>>.

%% create a random binary
randb() ->
	{X, S} = random:uniform_s((?MAXIFRAMESZ - 8), now()),
	randb(S, [], X).
randb(_, L, 0) ->
	list_to_binary(L);
randb(S0, L, N) ->
	{X, S1} = random:uniform_s(256, S0),
	randb(S1, L ++ [X -1], N - 1).

