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

-record(state, {server, port, lapdid, iframe, interval, interval_ref, hourly_ref}).

init([ServerRef, LapdId, Interval]) ->
	case ServerRef of
		{local, Name} ->
			ServerPid = whereis(Name);
		{global, Name} ->
			ServerPid = global:whereis_name(Name);
		Pid when is_pid(Pid) ->
			ServerPid = Pid;
		Name when is_atom(Name) ->
			ServerPid = whereis(Name)
	end,
	% open a lapid on the board
	case netaccess:open(ServerPid) of
		Port when is_port(Port) ->
			StateData = #state{server = ServerPid,
					port = Port, lapdid = LapdId,
					iframe = iframe(),
					interval = Interval},
			init_protocol(StateData);
		{'EXIT', Reason} ->
			{stop, Reason}
	end.

init_protocol(StateData) ->
 	L1 = #level1{l1_mode = ?IISDNl1modHDLC,
			num_txbuf = 4, num_rxbuf = 4},
	L2Parms = #l2_lap_params{mode = ?IISDNl2modLAP_D,
			dce_dte = ?IISDNdirSYMMETRIC, l2_detail = 1},
	D = #data_interface{enable = 1},
 	L2 = #level2{par = L2Parms, data_interface = D},
	ProtoData = #ena_proto_data{level1 = L1, level2 = L2},
	% send an L4L3mENABLE_PROTOCOL to start LAPD 
	netaccess:enable_protocol(StateData#state.port, 
			StateData#state.lapdid, ProtoData),
	H_ref = gen_fsm:start_timer(3600000, hourly),
	NewStateData = StateData#state{hourly_ref = H_ref},
	{ok, establishing, NewStateData}.


%% waiting to establish multiframe state
establishing({Channel, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mPROTOCOL_STATUS ->
	P = iisdn:protocol_stat(L3L4m#l3_to_l4.data),
	case 	P#protocol_stat.status of
		?IISDNdsESTABLISHING ->
			report_status(P, StateData#state.lapdid),
			{next_state, establishing, StateData};
		?IISDNdsNOT_ESTABLISHED ->
			report_status(P, StateData#state.lapdid),
			{next_state, not_established, StateData};
		?IISDNdsESTABLISHED ->
			report_status(P, StateData#state.lapdid),
			{Delay, _} = random:uniform_s(StateData#state.interval, now()),
			I_ref = gen_fsm:start_timer(Delay, interval),
			NewStateData = StateData#state{interval_ref = I_ref},
			{next_state, established, NewStateData}
	end;
establishing({Channel, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mL2_STATS ->
	print_stats(L3L4m#l3_to_l4.data),
	{next_state, establishing, StateData};
establishing({Channel, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mERROR ->
	Reason = iisdn:error_code(L3L4m#l3_to_l4.data),
	{stop, Reason, StateData};
establishing({timeout, _Ref, hourly}, StateData) ->
	netaccess:req_l2_stats(StateData#state.port, StateData#state.lapdid),
	H_ref = gen_fsm:start_timer(3600000, hourly),
	{next_state, establishing, StateData#state{hourly_ref = H_ref}};
establishing(Other, StateData) ->
	error_logger:info_report(["Message not handled",
			{lapdid, StateData#state.lapdid}, {state, establishing}, Other]),
	{next_state, establishing, StateData}.


%% multiframe state established
established({Channel, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mPROTOCOL_STATUS ->
	P = iisdn:protocol_stat(L3L4m#l3_to_l4.data),
	case 	P#protocol_stat.status of
		?IISDNdsESTABLISHED ->
			report_status(P, StateData#state.lapdid),
			{next_state, established, StateData};
		?IISDNdsESTABLISHING ->
			gen_fsm:cancel_timer(StateData#state.interval_ref),
			report_status(P, StateData#state.lapdid),
			{next_state, establishing, StateData};
		?IISDNdsNOT_ESTABLISHED ->
			gen_fsm:cancel_timer(StateData#state.interval_ref),
			report_status(P, StateData#state.lapdid),
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
established({timeout, _Ref, interval}, StateData) ->
	% send an IFRAME
	netaccess:send(StateData#state.port, StateData#state.iframe),
	I_ref = gen_fsm:start_timer(StateData#state.interval, interval),
	{next_state, established, StateData#state{interval_ref = I_ref}};
established({timeout, _Ref, hourly}, StateData) ->
	netaccess:req_l2_stats(StateData#state.port, StateData#state.lapdid),
	H_ref = gen_fsm:start_timer(3600000, hourly),
	{next_state, establishing, StateData#state{hourly_ref = H_ref}};
established({Channel, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mL2_STATS ->
	print_stats(L3L4m#l3_to_l4.data),
	{next_state, established, StateData};
established({Channel, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mERROR ->
	Reason = iisdn:error_code(L3L4m#l3_to_l4.data),
	{stop, Reason, StateData};
established(Other, StateData) ->
	error_logger:info_report(["Message not handled",
			{lapdid, element(2, StateData)}, {state, established}, Other]),
	{next_state, established, StateData}.

%% multiframe not established
not_established({Channel, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mPROTOCOL_STATUS ->
	P = iisdn:protocol_stat(L3L4m#l3_to_l4.data),
	case 	P#protocol_stat.status of
		?IISDNdsESTABLISHING ->
			report_status(P, StateData#state.lapdid),
			{next_state, establishing, StateData};
		?IISDNdsESTABLISHED ->
			report_status(P, StateData#state.lapdid),
			{Delay, _} = random:uniform_s(StateData#state.interval, now()),
			I_ref = gen_fsm:start_timer(Delay, interval),
			{next_state, established, StateData#state{interval_ref = I_ref}};
		?IISDNdsNOT_ESTABLISHED ->
			report_status(P, StateData#state.lapdid),
			{next_state, not_established, StateData}
	end;
not_established({Channel, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mL2_STATS ->
	print_stats(L3L4m#l3_to_l4.data),
	{next_state, not_established, StateData};
not_established({Channel, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mERROR ->
	Reason = iisdn:error_code(L3L4m#l3_to_l4.data),
	{stop, Reason, StateData};
not_established({timeout, _Ref, hourly}, StateData) ->
	netaccess:req_l2_stats(StateData#state.port, StateData#state.lapdid),
	H_ref = gen_fsm:start_timer(3600000, hourly),
	{next_state, establishing, StateData#state{hourly_ref = H_ref}};
not_established(Other, StateData) ->
	error_logger:info_report(["Message not handled",
			{lapdid, StateData#state.lapdid}, {state, not_established}, Other]),
	{next_state, not_established, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.
	
handle_sync_event(_Event, _From, StateName, StateData) ->
	{next_state, StateName, StateData}.
	
handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(Reason, StateName, StateData) ->
	error_logger:info_report([{lapdid, StateData#state.lapdid},
			{statename, StateName}, {reason, Reason}]),
	catch netaccess:close(StateData#state.port),
	catch gen_fsm:cancel_timer(StateData#state.interval_ref),
	gen_fsm:cancel_timer(StateData#state.hourly_ref).

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

report_status(P, LapdId) ->
	Status = case P#protocol_stat.status of
		?IISDNdsNOT_ESTABLISHED ->
			not_established;
		?IISDNdsESTABLISHING ->
			establishing;
		?IISDNdsESTABLISHED ->
			established;
		_ ->
			unknown
	end,
	State = case P#protocol_stat.l2_state of
		?IISDNlpdsTEI_UNASSIGNED ->
			tei_unassigned;
		?IISDNlpdsTEI_ASSIGNED ->
			tei_assigned;
		?IISDNlpdsAWAITING_ESTABLISHMENT ->
			awaiting_establishment;
		?IISDNlpdsAWAITING_RELEASE ->
			awaiting_release;
		?IISDNlpdsMULTIFRAME_ESTABLISHED ->
			multiframe_established;
		?IISDNlpdsTIMER_RECOVERY ->
			timer_recovery;
		_ ->
			unknown
	end,
	Error = case P#protocol_stat.l2_error of
		?IISDNl2errNO_ERROR ->
			none;
		?IISDNl2errA ->
			"Supervisory (F=1)";
		?IISDNl2errB ->
			"DM (F=1)";
		?IISDNl2errC ->
			"UA (F=1)";
		?IISDNl2errD ->
			"UA (F=0)";
		?IISDNl2errE ->
			"DM (F=0)";
		?IISDNl2errF ->
			"SABME received";
		?IISDNl2errG ->
			"SABME";
		?IISDNl2errH ->
			"DISC";
		?IISDNl2errI ->
			"Status Enquiry";
		?IISDNl2errJ ->
			"N(R) error";
		?IISDNl2errK ->
			"FRMR received";
		?IISDNl2errL ->
			"Receipt of unimplemented frame";
		?IISDNl2errM ->
			"Receipt of I field not permitted";
		?IISDNl2errN ->
			"Receipt of frame with wrong size";
		?IISDNl2errO ->
			"N201 error";
		_ ->
			unknown
	end,
	Detail = case P#protocol_stat.l2_detail of
		?IISDNdsmskDM_RCVD ->
			"DM received with F bit set";
		?IISDNdsmskSABME_RCVD ->
			"SABME/SABM received";
		?IISDNdsmskSABME_SENT ->
			"SABME/SABM sent";
		?IISDNdsmskFRAME_MOD_8 ->
			"Received frame with non-integral octets";
		?IISDNdsmskBAD_CRC ->
			"Received frame with bad CRC";
		?IISDNdsmskBAD_LEN ->
			"Received frame with bad length";
		?IISDNdsmskUNKN_CTRL ->
			"Received frame with unknown control field";
		?IISDNdsmskUNKN_DLCI ->
			"Received frame with unknown address";
		?IISDNdsmskUNEXPECTED ->
			"Received valid message type in bad state";
		?IISDNdsmskDISC_RCVD ->
			"Received disconnect message";
		?IISDNdsmskT200 ->
			"T200/N200 timeout";
		?IISDNdsmskXID_RCVD ->
			"XID received (V.120 UI mode)";
		?IISDNdsmskUA_RCVD ->
			"UA received";
		?IISDNdsmskONR_ERROR ->
			"Bt8474/8 ONR error";
		_ ->
			unknown
	end,
	error_logger:info_report([{lapdid, LapdId},
			{status, Status},
			{l2_state, State},
			{l2_error, Error},
			{l2_errpt, P#protocol_stat.l2_errpt},
			{txcount, P#protocol_stat.txcount},
			{rxcount, P#protocol_stat.rxcount},
			{l2_detail, Detail},
			{l2_detail_data, P#protocol_stat.l2_detail_data}]).

print_stats(L2Mtp2StatsBin) when is_binary(L2Mtp2StatsBin) ->
	print_stats(iisdn:l2_mtp2_stats(L2Mtp2StatsBin));
print_stats(L2Mtp2StatsRec) when is_record(L2Mtp2StatsRec, l2_mtp2_stats) ->
	print_l2_stats(iisdn:l2_stats(L2Mtp2StatsRec#l2_mtp2_stats.stats)).
print_l2_stats(L2Stats) when is_record(L2Stats, l2_stats) ->
	error_logger:info_report(["Level 2 Statistics",
			{iframe_tx, L2Stats#l2_stats.iframe_tx},
			{rr_cmd_tx, L2Stats#l2_stats.rr_cmd_tx},
			{rnr_cmd_tx, L2Stats#l2_stats.rnr_cmd_tx},
			{rej_cmd_tx, L2Stats#l2_stats.rej_cmd_tx},
			{sabm_tx, L2Stats#l2_stats.sabm_tx},
			{sabme_tx, L2Stats#l2_stats.sabme_tx},
			{disc_tx, L2Stats#l2_stats.disc_tx},
			{rr_rsp_tx, L2Stats#l2_stats.rr_rsp_tx},
			{rnr_rsp_tx, L2Stats#l2_stats.rnr_rsp_tx},
			{rej_rsp_tx, L2Stats#l2_stats.rej_rsp_tx},
			{dm_tx, L2Stats#l2_stats.dm_tx},
			{ua_tx, L2Stats#l2_stats.ua_tx},
			{frmr_tx, L2Stats#l2_stats.frmr_tx},
			{iframe_rx, L2Stats#l2_stats.iframe_rx},
			{rr_cmd_rx, L2Stats#l2_stats.rr_cmd_rx},
			{rnr_cmd_rx, L2Stats#l2_stats.rnr_cmd_rx},
			{rej_cmd_rx, L2Stats#l2_stats.rej_cmd_rx},
			{sabm_rx, L2Stats#l2_stats.sabm_rx},
			{sabme_rx, L2Stats#l2_stats.sabme_rx},
			{disc_rx, L2Stats#l2_stats.disc_rx},
			{rr_rsp_rx, L2Stats#l2_stats.rr_rsp_rx},
			{rnr_rsp_rx, L2Stats#l2_stats.rnr_rsp_rx},
			{rej_rsp_rx, L2Stats#l2_stats.rej_rsp_rx},
			{dm_rx, L2Stats#l2_stats.dm_rx},
			{ua_rx, L2Stats#l2_stats.ua_rx},
			{frmr_rx, L2Stats#l2_stats.frmr_rx},
			{crc_errors, L2Stats#l2_stats.crc_errors},
			{rcv_errors, L2Stats#l2_stats.rcv_errors},
			{retrans_cnt, L2Stats#l2_stats.retrans_cnt},
			{poll_errors, L2Stats#l2_stats.poll_errors},
			{ui_tx, L2Stats#l2_stats.ui_tx},
			{ui_rx, L2Stats#l2_stats.ui_rx}]).
	
