%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% iisdn.erl   Erlang module to encode/decode iisdn structures/records %%%
%%%                                                                     %%%
%%%---------------------------------------------------------------------%%%
%%% Copyright Motivity Telecom Inc. 2001, 2002, 2003                    %%%
%%%                                                                     %%%
%%% All rights reserved. No part of this computer program(s) may be     %%%
%%% used, reproduced, stored in any retrieval system, or transmitted,   %%%
%%% in any form or by any means, electronic, mechanical, photocopying,  %%%
%%% recording, or otherwise without prior written permission of         %%%
%%% Motivity Telecom Inc.                                               %%%
%%%---------------------------------------------------------------------%%%
%%%                                                                     %%%
%%% For every structure definition in iisdn.h there should be here:     %%%
%%%                                                                     %%%
%%%        - a record definition (e.g. #q931_cnfg{})                    %%%
%%%                                                                     %%%
%%%        - a function (e.g. #q931_cnfg(Q931) -> binary())             %%%
%%%                                                                     %%%
%%% A received binary is decoded to a record, or a record is encoded    %%%
%%% to a binary with the function of the same name as the structure     %%%
%%% using the record definition of the same name.                       %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(iisdn).

-export([l3_to_l4/1, l4_to_l3/1]).
-export([ena_proto_data/1]).
-export([hardware_data/1, line_data/1]).
-export([tsi_data/1, tsi_map/1]).
-export([q931_timers/1]).
-export([data_interface_configuration/1]).
-export([level1_cnfg/1, level2_cnfg/1, level3_cnfg/1]).
-export([l2_lap_params/1, l2_ss7_params/1, l2_udpip_params/1,
		l2_dpnss_params/1, l2_v110_params/1]).
-export([data_interface/1]).
-export([q931_cnfg/1, bonding_data/1, x25_config/1, pm_config/1,
		relay_config/1, dpnsscc_config/1, dasscc_config/1,
		q933a_config/1]).
-export([l2_lap_consts/1, l2_ss7_consts/1, l2_ip_consts/1,
		l2_dpnss_consts/1]).

-include("iisdn.hrl").


%%
%% L4_to_L3_struct
%%
%% This function takes a record of the same name and returns a binary
%% representation of an entire SMI L4L3 message suitable for sending to
%% the boards.  If the data member is assigned a binary it will be sent
%% as is.  If it is undefined a default record of the type associated
%% with msgtype is used.  If data is a record of the appropriate type
%% it will be used to build the binary.
%%
%% The first octet is codes as a zero for all L4L3 messages so that the
%% erlang netaccess driver knows to send them as control messages on the
%% stream (data messages such as I-frames have a non-zero first octet).
%%
l4_to_l3(R) when is_record(R, l4_to_l3) ->
	MessageType = R#l4_to_l3.msgtype, 
	CommonHeader = <<(R#l4_to_l3.lapdid):?IISDNu8bit, 
			(R#l4_to_l3.msgtype):?IISDNu8bit,
			(R#l4_to_l3.l4_ref):?IISDNu16bit,
			(R#l4_to_l3.call_ref):?IISDNu16bit,
			(R#l4_to_l3.lli):?IISDNu16bit>>,
	MessageSpecificData = R#l4_to_l3.data,
	l4_to_l3(MessageType, CommonHeader, MessageSpecificData).
l4_to_l3(_, Header, Data) when is_binary(Data) ->
	<<0, Header/binary, Data/binary>>;
l4_to_l3(?L4L3mSET_HARDWARE, Header, Data) ->
	<<0, Header/binary, (hardware_data(Data))/binary>>;
l4_to_l3(?L4L3mSET_TSI, Header, Data) ->
	<<0, Header/binary, (tsi_data(Data))/binary>>;
l4_to_l3(?L4L3mENABLE_PROTOCOL, Header, Data) ->
	<<0, Header/binary, (ena_proto_data(Data))/binary>>.

l3_to_l4(Bin) when is_binary(Bin) ->
	<<Lapdid:?IISDNu8bit, Msgtype:?IISDNu8bit, L4_ref:?IISDNu16bit,
		Call_ref:?IISDNu16bit, Bchanel:?IISDNu8bit, Iface:?IISDNu8bit,
		Bchannel_mask:?IISDNu32bit, Lli:?IISDNu16bit, Data_channel:?IISDNu16bit,
		Data/binary>> = Bin,
	#l3_to_l4{lapdid=Lapdid, msgtype=Msgtype, l4_ref=L4_ref,
			call_ref=Call_ref, bchanel=Bchanel, iface=Iface,
			bchannel_mask=Bchannel_mask, lli=Lli, 
			data_channel=Data_channel, data=Data}.

level1_cnfg(L1) when is_record(L1, level1_cnfg) ->
	<<(L1#level1_cnfg.l1_mode):?IISDNu8bit,
			(L1#level1_cnfg.invert_hdlc):?IISDNu8bit,
	      (L1#level1_cnfg.num_txbuf):?IISDNu16bit,
	      (L1#level1_cnfg.num_rxbuf):?IISDNu16bit,
	      (L1#level1_cnfg.buffsz):?IISDNu16bit,
	      (L1#level1_cnfg.chain):?IISDNu8bit,
	      (L1#level1_cnfg.device):?IISDNu8bit,
	      (L1#level1_cnfg.bit_reverse):?IISDNu8bit,
	      (L1#level1_cnfg.vme_lock):?IISDNu8bit,
	      (L1#level1_cnfg.hdlc_channels):?IISDNu32bit,
	      (L1#level1_cnfg.chan_kbit_rate):?IISDNu16bit,
	      (L1#level1_cnfg.crc_bytes):?IISDNu8bit,
	      (L1#level1_cnfg.crc_ignore_errs):?IISDNu8bit,
			((L1#level1_cnfg.rate_adapt)#rate_adapt.enable):?IISDNu8bit,
			((L1#level1_cnfg.rate_adapt)#rate_adapt.rate_adapt_value):?IISDNu8bit,
			0:?IISDNu16bit,
			((L1#level1_cnfg.raw_fillchar)#raw_fillchar.enable):?IISDNu8bit,
			((L1#level1_cnfg.raw_fillchar)#raw_fillchar.fill_value):?IISDNu8bit,
			0:?IISDNu16bit,
			((L1#level1_cnfg.hdlc_flag_fill)#hdlc_flag_fill.enable):?IISDNu8bit,
			((L1#level1_cnfg.hdlc_flag_fill)#hdlc_flag_fill.mode):?IISDNu8bit,
			((L1#level1_cnfg.hdlc_flag_fill)#hdlc_flag_fill.value):?IISDNu16bit,
			((L1#level1_cnfg.modem)#modem.originate):?IISDNu8bit,
			((L1#level1_cnfg.modem)#modem.faxClass):?IISDNu8bit,
			((L1#level1_cnfg.modem)#modem.encoding):?IISDNu8bit,
			((L1#level1_cnfg.modem)#modem.amf):?IISDNu8bit,
			(((L1#level1_cnfg.modem)#modem.amf_params)#amf_params.'0'):?IISDNu32bit,
			(((L1#level1_cnfg.modem)#modem.amf_params)#amf_params.'1'):?IISDNu32bit,
			(((L1#level1_cnfg.modem)#modem.amf_params)#amf_params.'2'):?IISDNu32bit,
			(((L1#level1_cnfg.modem)#modem.amf_params)#amf_params.'3'):?IISDNu32bit,
			((L1#level1_cnfg.modem)#modem.minBPS):?IISDNu32bit,
			((L1#level1_cnfg.modem)#modem.maxBPS):?IISDNu32bit,
			((L1#level1_cnfg.v110)#v110.bit_rate):?IISDNu32bit,
			((L1#level1_cnfg.v110)#v110.auto_detect):?IISDNu8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit>>.
			

level2_cnfg(L2) when is_record(L2, level2_cnfg),
		is_record(L2#level2_cnfg.par, l2_lap_params) ->
	level2_cnfg(L2#level2_cnfg{
			par = l2_lap_params(L2#level2_cnfg.par)});
level2_cnfg(L2) when is_record(L2, level2_cnfg),
		is_record(L2#level2_cnfg.par, l2_ss7_params) ->
	level2_cnfg(L2#level2_cnfg{
			par = l2_ss7_params(L2#level2_cnfg.par)});
level2_cnfg(L2) when is_record(L2, level2_cnfg),
		is_record(L2#level2_cnfg.par, l2_udpip_params) ->
	level2_cnfg(L2#level2_cnfg{
			par = l2_udpip_params(L2#level2_cnfg.par)});
level2_cnfg(L2) when is_record(L2, level2_cnfg),
		is_record(L2#level2_cnfg.par, l2_dpnss_params) ->
	level2_cnfg(L2#level2_cnfg{
			par = l2_dpnss_params(L2#level2_cnfg.par)});
level2_cnfg(L2) when is_record(L2, level2_cnfg),
		is_record(L2#level2_cnfg.par, l2_v110_params) ->
	level2_cnfg(L2#level2_cnfg{
			par = l2_v110_params(L2#level2_cnfg.par)});
level2_cnfg(L2) when is_record(L2, level2_cnfg),
		is_record(L2#level2_cnfg.data_interface, data_interface) ->
	level2_cnfg(L2#level2_cnfg{
			data_interface = 
					data_interface(L2#level2_cnfg.data_interface)});
level2_cnfg(L2) when is_record(L2, level2_cnfg),
		is_record(L2#level2_cnfg.consts, l2_lap_consts) ->
	level2_cnfg(L2#level2_cnfg{
			consts = l2_lap_consts(L2#level2_cnfg.consts)});
level2_cnfg(L2) when is_record(L2, level2_cnfg),
		is_record(L2#level2_cnfg.consts, l2_ss7_consts) ->
	level2_cnfg(L2#level2_cnfg{
			consts = l2_ss7_consts(L2#level2_cnfg.consts)});
level2_cnfg(L2) when is_record(L2, level2_cnfg),
		is_record(L2#level2_cnfg.consts, l2_ip_consts) ->
	level2_cnfg(L2#level2_cnfg{
			consts = l2_ip_consts(L2#level2_cnfg.consts)});
level2_cnfg(L2) when is_record(L2, level2_cnfg),
		is_record(L2#level2_cnfg.consts, l2_dpnss_consts) ->
	level2_cnfg(L2#level2_cnfg{
			consts = l2_dpnss_consts(L2#level2_cnfg.consts)});
level2_cnfg(L2) when is_record(L2, level2_cnfg),
		is_binary(L2#level2_cnfg.par);
		is_binary(L2#level2_cnfg.data_interface);
		is_binary(L2#level2_cnfg.consts) ->
	<<(L2#level2_cnfg.par)/binary, 
			(L2#level2_cnfg.data_interface)/binary,
			(L2#level2_cnfg.consts)/binary>>.


data_interface(DataIf) when is_record(DataIf, data_interface) ->
	<<(DataIf#data_interface.enable):?IISDNu8bit,
			(DataIf#data_interface.data_channel):?IISDNu8bit,
			(DataIf#data_interface.fillandspill):?IISDNu8bit,
			(DataIf#data_interface.allow_buffer_preload):?IISDNu8bit>>.

l2_lap_params(Lap) when is_record(Lap, l2_lap_params) ->
	<<(Lap#l2_lap_params.mode):?IISDNu8bit,
			(Lap#l2_lap_params.dce_dte):?IISDNu8bit,
			(Lap#l2_lap_params.tei_mode):?IISDNu8bit,
			(Lap#l2_lap_params.no_sabme):?IISDNu8bit,
			(Lap#l2_lap_params.l2_detail):?IISDNu8bit,
			(Lap#l2_lap_params.timestamp):?IISDNu8bit,
			(Lap#l2_lap_params.ui_mode):?IISDNu8bit,
			(Lap#l2_lap_params.priority):?IISDNu8bit,
			(Lap#l2_lap_params.no_reestab):?IISDNu8bit,
			(Lap#l2_lap_params.mode_1tr6):?IISDNu8bit,
			(Lap#l2_lap_params.mode_tei_1):?IISDNu8bit,
			(Lap#l2_lap_params.no_piggyback):?IISDNu8bit>>.

l2_ss7_params(Mtp2) when is_record(Mtp2, l2_ss7_params) ->
	<<(Mtp2#l2_ss7_params.mode):?IISDNu8bit,
			(Mtp2#l2_ss7_params.variant):?IISDNu8bit,
			0:?IISDNu16bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit>>.

l2_udpip_params(Udpip) when is_record(Udpip, l2_udpip_params) ->
	<<(Udpip#l2_udpip_params.mode):?IISDNu8bit,
			0:?IISDNu8bit,
			(Udpip#l2_udpip_params.dstport):?IISDNu16bit,
			(Udpip#l2_udpip_params.dstipaddr):?IISDNu32bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit>>.

l2_dpnss_params(Dpnss) when is_record(Dpnss, l2_dpnss_params) ->
	<<(Dpnss#l2_dpnss_params.mode):?IISDNu8bit,
			(Dpnss#l2_dpnss_params.pbx_b):?IISDNu8bit,
			(Dpnss#l2_dpnss_params.sabmr_as_ack):?IISDNu8bit,
			(Dpnss#l2_dpnss_params.tie_line_mode):?IISDNu8bit>>.

l2_v110_params(V110) when is_record(V110, l2_v110_params) ->
	<<(V110#l2_v110_params.mode):?IISDNu8bit,
			(V110#l2_v110_params.ebits):?IISDNu8bit,
			(V110#l2_v110_params.flow_control):?IISDNu8bit,
			(V110#l2_v110_params.nine_byte_rx_frames):?IISDNu8bit,
			(V110#l2_v110_params.num_tx_idle_frames):?IISDNu16bit,
			(V110#l2_v110_params.max_rx_frame_size):?IISDNu16bit,
			(V110#l2_v110_params.stale_rx_data_timer):?IISDNu16bit,
			(V110#l2_v110_params.filter_status_messages):?IISDNu8bit,
			0:?IISDNu8bit>>.
	
l2_lap_consts(L2) when is_record(L2, l2_lap_consts) ->
	<<(L2#l2_lap_consts.t200):?IISDNu16bit,
			(L2#l2_lap_consts.t201):?IISDNu16bit,
			(L2#l2_lap_consts.t202):?IISDNu16bit,
			(L2#l2_lap_consts.t203):?IISDNu16bit,
			(L2#l2_lap_consts.n200):?IISDNu16bit,
			(L2#l2_lap_consts.n201):?IISDNu16bit,
			(L2#l2_lap_consts.n202):?IISDNu16bit,
			(L2#l2_lap_consts.k):?IISDNu16bit>>.

l2_ss7_consts(Mtp2) when is_record(Mtp2, l2_ss7_consts) ->
	<<(Mtp2#l2_ss7_consts.t1):?IISDNu16bit,
			(Mtp2#l2_ss7_consts.t2):?IISDNu16bit,
			(Mtp2#l2_ss7_consts.t3):?IISDNu16bit,
			(Mtp2#l2_ss7_consts.t4n):?IISDNu16bit,
			(Mtp2#l2_ss7_consts.t4e):?IISDNu16bit,
			(Mtp2#l2_ss7_consts.t5):?IISDNu16bit,
			(Mtp2#l2_ss7_consts.t6):?IISDNu16bit,
			(Mtp2#l2_ss7_consts.t7):?IISDNu16bit>>.

l2_ip_consts(Ip) when is_record(Ip, l2_ip_consts) ->
	<<(Ip#l2_ip_consts.no_dhcp):?IISDNu8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit,
			(Ip#l2_ip_consts.ipaddr):?IISDNu32bit,
			(Ip#l2_ip_consts.gwaddr):?IISDNu32bit,
			(Ip#l2_ip_consts.subnet_mask):?IISDNu32bit>>.

l2_dpnss_consts(Dpnss) when is_record(Dpnss, l2_dpnss_consts) ->
	<<(Dpnss#l2_dpnss_consts.nl):?IISDNu16bit,
			0:?IISDNu16bit,
			(Dpnss#l2_dpnss_consts.nt1):?IISDNu32bit,
			(Dpnss#l2_dpnss_consts.nt2):?IISDNu32bit>>.


level3_cnfg(L3) when is_record(L3, level3_cnfg),
		is_record(L3#level3_cnfg.cnfg, q931_cnfg) ->
	Bin = q931_cnfg(L3#level3_cnfg.cnfg),
	level3_cnfg(L3#level3_cnfg{cnfg = Bin});
level3_cnfg(L3) when is_record(L3, level3_cnfg),
		is_record(L3#level3_cnfg.cnfg, bonding_data) ->
	Bin = bonding_data(L3#level3_cnfg.cnfg),
	level3_cnfg(L3#level3_cnfg{cnfg = Bin});
level3_cnfg(L3) when is_record(L3, level3_cnfg),
		is_record(L3#level3_cnfg.cnfg, x25_config) ->
	Bin = x25_config(L3#level3_cnfg.cnfg),
	level3_cnfg(L3#level3_cnfg{cnfg = Bin});
level3_cnfg(L3) when is_record(L3, level3_cnfg),
		is_record(L3#level3_cnfg.cnfg, pm_config) ->
	Bin = pm_config(L3#level3_cnfg.cnfg),
	level3_cnfg(L3#level3_cnfg{cnfg = Bin});
level3_cnfg(L3) when is_record(L3, level3_cnfg),
		is_record(L3#level3_cnfg.cnfg, relay_config) ->
	Bin = relay_config(L3#level3_cnfg.cnfg),
	level3_cnfg(L3#level3_cnfg{cnfg = Bin});
level3_cnfg(L3) when is_record(L3, level3_cnfg),
		is_record(L3#level3_cnfg.cnfg, dpnsscc_config) ->
	Bin = dpnsscc_config(L3#level3_cnfg.cnfg),
	level3_cnfg(L3#level3_cnfg{cnfg = Bin});
level3_cnfg(L3) when is_record(L3, level3_cnfg),
		is_record(L3#level3_cnfg.cnfg, dasscc_config) ->
	Bin = dasscc_config(L3#level3_cnfg.cnfg),
	level3_cnfg(L3#level3_cnfg{cnfg = Bin});
level3_cnfg(L3) when is_record(L3, level3_cnfg),
		is_record(L3#level3_cnfg.cnfg, q933a_config) ->
	Bin = q933a_config(L3#level3_cnfg.cnfg),
	level3_cnfg(L3#level3_cnfg{cnfg = Bin});
level3_cnfg(L3) when is_record(L3, level3_cnfg),
		is_binary(L3#level3_cnfg.cnfg) ->
	<<(L3#level3_cnfg.l3_mode):?IISDNu8bit, 0:?IISDNu8bit,
			0:?IISDNu16bit, (L3#level3_cnfg.cnfg)/binary>>.


q931_cnfg(Q931) when is_record(Q931, q931_cnfg) ->
	Q931_Timers = q931_timers(Q931#q931_cnfg.q931_timers),
	Digit32 = fun(Digit, Bin) -> <<Bin/binary, Digit:?IISDNu32bit>> end,
	B_channel_service_state = lists:foldl(Digit32, <<>>,
			Q931#q931_cnfg.b_channel_service_state),
	Digit8 = fun(Digit, Bin) -> <<Bin/binary, Digit:?IISDNu8bit>> end,
	Spid = lists:foldl(Digit8, <<>>, Q931#q931_cnfg.spid),
	Spid_1 = lists:foldl(Digit8, <<>>, Q931#q931_cnfg.spid_1),
	Dn = lists:foldl(Digit8, <<>>, Q931#q931_cnfg.dn),
	Dn_1 = lists:foldl(Digit8, <<>>, Q931#q931_cnfg.dn_1),
	<<(Q931#q931_cnfg.switch_type):?IISDNu16bit,
			(Q931#q931_cnfg.variant):?IISDNu16bit,
			(Q931#q931_cnfg.call_filtering):?IISDNu32bit,
			B_channel_service_state/binary,
			(Q931#q931_cnfg.nfas):?IISDNu8bit,
			(Q931#q931_cnfg.e1_30_bchan):?IISDNu8bit,
			(Q931#q931_cnfg.basic_rate):?IISDNu8bit,
			(Q931#q931_cnfg.net_side_emul):?IISDNu8bit,
			(Q931#q931_cnfg.b_chan_negot):?IISDNu8bit,
			(Q931#q931_cnfg.proc_on_exclusv):?IISDNu8bit,
			(Q931#q931_cnfg.chanid_slot_map):?IISDNu8bit,
			(Q931#q931_cnfg.sprs_chanid_callproc):?IISDNu8bit,
			(Q931#q931_cnfg.no_chanid_callproc):?IISDNu8bit,
			(Q931#q931_cnfg.append_raw_qmsg):?IISDNu8bit,
			(Q931#q931_cnfg.ccitt_mode):?IISDNu8bit,
			(Q931#q931_cnfg.raw_qmsg):?IISDNu8bit,
			(Q931#q931_cnfg.no_ie_errcheck):?IISDNu8bit,
			(Q931#q931_cnfg.user_ie_encode):?IISDNu8bit,
			(Q931#q931_cnfg.overlap_rcv):?IISDNu8bit,
			(Q931#q931_cnfg.send_l3l4_callproc):?IISDNu8bit,
			(Q931#q931_cnfg.sending_cmplt):?IISDNu8bit,
			(Q931#q931_cnfg.require_send_complete):?IISDNu8bit,
			(Q931#q931_cnfg.report_incoming_callproc):?IISDNu8bit,
			(Q931#q931_cnfg.no_tx_conn_ack):?IISDNu8bit,
			(Q931#q931_cnfg.no_rx_conn_ack):?IISDNu8bit,
			(Q931#q931_cnfg.sprs_chanid_setupack):?IISDNu8bit,
			(Q931#q931_cnfg.no_chanid_setupack):?IISDNu8bit,
			(Q931#q931_cnfg.no_canned_spid_rej):?IISDNu8bit,
			(Q931#q931_cnfg.call_reject_notify):?IISDNu8bit,
			(Q931#q931_cnfg.advice_of_charge):?IISDNu8bit,
			(Q931#q931_cnfg.message_segmentation):?IISDNu8bit,
			(Q931#q931_cnfg.no_bc_user_info):?IISDNu8bit,
			(Q931#q931_cnfg.incoming_call_slot_map):?IISDNu8bit,
			(Q931#q931_cnfg.release_complete_control):?IISDNu8bit,
			(Q931#q931_cnfg.primary_lapdid):?IISDNu8bit,
			(Q931#q931_cnfg.backup_lapdid):?IISDNu8bit,
			0:?IISDNu16bit,
			(Q931#q931_cnfg.primary_ifnum):?IISDNu8bit,
			(Q931#q931_cnfg.backup_ifnum):?IISDNu8bit,
			(Q931#q931_cnfg.backup_control):?IISDNu8bit,
			(Q931#q931_cnfg.spid_len):?IISDNu8bit,
			(Q931#q931_cnfg.spid_1_len):?IISDNu8bit,
			(Q931#q931_cnfg.dn_len):?IISDNu8bit,
			Spid/binary, Spid_1/binary, Dn/binary, Dn_1/binary,
			(Q931#q931_cnfg.chan_id_high_bit):?IISDNu8bit,
			(Q931#q931_cnfg.att_cust_bri_ekts):?IISDNu8bit,
			(Q931#q931_cnfg.subscribe_connack):?IISDNu8bit,
			(Q931#q931_cnfg.suppress_auto_spid):?IISDNu8bit,
			(Q931#q931_cnfg.accept_all_bri_calls):?IISDNu8bit,
			0:?IISDNu16bit>>.

bonding_data(Bond) when is_record(Bond, bonding_data) ->
	Digit32 = fun(Digit, Bin) -> <<Bin/binary, Digit:?IISDNu32bit>> end,
	Directory = lists:foldl(Digit32, <<>>, 
			Bond#bonding_data.directory),
	<<(Bond#bonding_data.mode):?IISDNu16bit,
			(Bond#bonding_data.destination):?IISDNu8bit,
			(Bond#bonding_data.num_tx_buf):?IISDNu8bit,
			(Bond#bonding_data.num_rx_buf):?IISDNu8bit,
			(Bond#bonding_data.data_channel):?IISDNu8bit,
			(Bond#bonding_data.txinit):?IISDNu16bit,
			(Bond#bonding_data.txadd01):?IISDNu16bit,
			(Bond#bonding_data.txfa):?IISDNu16bit,
			(Bond#bonding_data.txdisc):?IISDNu16bit,
			(Bond#bonding_data.txdeq):?IISDNu16bit,
			(Bond#bonding_data.tcid):?IISDNu16bit,
			(Bond#bonding_data.tanull):?IISDNu16bit,
			(Bond#bonding_data.channels):?IISDNu16bit,
			Directory/binary>>.
	
x25_config(X25) when is_record(X25, x25_config) ->
	<<(X25#x25_config.cfg_msk):?IISDNu32bit,
			(X25#x25_config.t10):?IISDNu16bit,
			(X25#x25_config.t11):?IISDNu16bit,
			(X25#x25_config.t12):?IISDNu16bit,
			(X25#x25_config.t13):?IISDNu16bit,
			(X25#x25_config.t28):?IISDNu16bit,
			(X25#x25_config.p):?IISDNu16bit,
			(X25#x25_config.w):?IISDNu8bit,
			(X25#x25_config.max_clr_retry):?IISDNu8bit,
			(X25#x25_config.max_svcs):?IISDNu8bit,
			(X25#x25_config.max_pvcs):?IISDNu8bit>>.

pm_config(PM) when is_record(PM, pm_config) ->
	Digit8 = fun(Digit, Bin) -> <<Bin/binary, Digit:?IISDNu8bit>> end,
	Equipmentid = lists:foldl(Digit8, <<>>, PM#pm_config.equipmentid),
	Locationid = lists:foldl(Digit8, <<>>, PM#pm_config.locationid),
	Frameid = lists:foldl(Digit8, <<>>, PM#pm_config.frameid),
	Unitid = lists:foldl(Digit8, <<>>, PM#pm_config.unitid),
	Facilityid = lists:foldl(Digit8, <<>>, PM#pm_config.facilityid),
	<<(PM#pm_config.mode):?IISDNu8bit, 
			(PM#pm_config.carrier):?IISDNu8bit,
			(PM#pm_config.fdl_alert):?IISDNu8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit,
			Equipmentid/binary, Locationid/binary, Frameid/binary,
			Unitid/binary, Facilityid/binary>>.

relay_config(Relay) when is_record(Relay, relay_config) ->
	<<(Relay#relay_config.default_dest):?IISDNu8bit,
			(Relay#relay_config.default_dest_id):?IISDNu8bit,
			(Relay#relay_config.default_root_idx):?IISDNu8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit>>.

dpnsscc_config(Dpnss) when is_record(Dpnss, dpnsscc_config) ->
	<<(Dpnss#dpnsscc_config.pbx_y):?IISDNs8bit,
			(Dpnss#dpnsscc_config.no_virtual_channels):?IISDNs8bit,
			(Dpnss#dpnsscc_config.dest_addr_len):?IISDNs8bit,
			0:?IISDNu8bit,
			(Dpnss#dpnsscc_config.b_channel_service_state):?IISDNu32bit,
			(Dpnss#dpnsscc_config.v_channel_service_state):?IISDNu32bit,
			(Dpnss#dpnsscc_config.t_i_msg):?IISDNs32bit,
			(Dpnss#dpnsscc_config.t_guard):?IISDNs32bit>>.

dasscc_config(Dass) when is_record(Dass, dasscc_config) ->
	<<(Dass#dasscc_config.b_channel_service_state):?IISDNu32bit,
			(Dass#dasscc_config.t_digit_racking):?IISDNs32bit,
			(Dass#dasscc_config.n_clear_retries):?IISDNs8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit>>.

q933a_config(Q933a) when is_record(Q933a, q933a_config) ->
	<<(Q933a#q933a_config.network_side):?IISDNu8bit,
			(Q933a#q933a_config.n391):?IISDNu8bit,
			(Q933a#q933a_config.n392):?IISDNu8bit,
			(Q933a#q933a_config.n393):?IISDNu8bit,
			(Q933a#q933a_config.t391):?IISDNu16bit,
			(Q933a#q933a_config.t392):?IISDNu16bit>>.


data_interface_configuration(D) 
			when is_record(D, data_interface_configuration) ->
	<<(D#data_interface_configuration.dchan_descr_addr):?IISDNp32bit,
			(D#data_interface_configuration.num_dchan_descr):?IISDNu16bit,
			0:?IISDNu16bit,
			(D#data_interface_configuration.dchan_event_queue_addr):?IISDNp16bit,
			(D#data_interface_configuration.num_l3l4_dchan_events):?IISDNu16bit,
			(D#data_interface_configuration.num_l4l3_dchan_events):?IISDNu16bit>>;
data_interface_configuration(DataIf) when is_binary(DataIf) ->
	<<Dchan_descr_addr:?IISDNp32bit, Num_dchan_descr:?IISDNu16bit,
			_:?IISDNu16bit,
			Dchan_event_queue_addr:?IISDNp16bit,
			Num_l3l4_dchan_events:?IISDNu16bit,
			Num_l4l3_dchan_events:?IISDNu16bit>> = DataIf,
	#data_interface_configuration{dchan_descr_addr=Dchan_descr_addr,
			num_dchan_descr=Num_dchan_descr,
			dchan_event_queue_addr=Dchan_event_queue_addr,
			num_l3l4_dchan_events=Num_l3l4_dchan_events,
			num_l4l3_dchan_events=Num_l4l3_dchan_events}.

ena_proto_data(Proto) when is_record(Proto, ena_proto_data),
		is_record(Proto#ena_proto_data.level1, level1_cnfg) ->
	ena_proto_data(Proto#ena_proto_data{
			level1 = level1_cnfg(Proto#ena_proto_data.level1)});
ena_proto_data(Proto) when is_record(Proto, ena_proto_data),
		is_record(Proto#ena_proto_data.level2, level2_cnfg) ->
	ena_proto_data(Proto#ena_proto_data{
			level2 = level2_cnfg(Proto#ena_proto_data.level2)});
ena_proto_data(Proto) when is_record(Proto, ena_proto_data),
		is_record(Proto#ena_proto_data.level3, level3_cnfg) ->
	ena_proto_data(Proto#ena_proto_data{
			level3 = level3_cnfg(Proto#ena_proto_data.level3)});
ena_proto_data(Proto) when is_record(Proto, ena_proto_data),
		is_binary(Proto#ena_proto_data.level1);
		is_binary(Proto#ena_proto_data.level2);
		is_binary(Proto#ena_proto_data.level3) ->
	<<(Proto#ena_proto_data.level1)/binary,
			(Proto#ena_proto_data.level2)/binary,
			(Proto#ena_proto_data.level3)/binary>>.

hardware_data(HW) when is_record(HW, hardware_data),
		is_binary(HW#hardware_data.line_data) ->
	Digit8 = fun(Digit8, Bin) -> <<Bin/binary, Digit8:?IISDNu8bit>> end,
	Csu = lists:foldl(Digit8, <<>>, HW#hardware_data.csu),
	<<(HW#hardware_data.clocking):?IISDNu8bit,
			(HW#hardware_data.clocking2):?IISDNu8bit,
			(HW#hardware_data.enable_clocking2):?IISDNu8bit,
			(HW#hardware_data.netref_clocking):?IISDNu8bit,
			(HW#hardware_data.netref_rate):?IISDNu8bit,
			(HW#hardware_data.ctbus_mode):?IISDNu8bit,
			(HW#hardware_data.force_framer_init):?IISDNu8bit,
			(HW#hardware_data.tdm_rate):?IISDNu8bit,
			(HW#hardware_data.enable_8370_rliu_monitor):?IISDNu8bit,
			(HW#hardware_data.dbcount):?IISDNu8bit,
			(HW#hardware_data.enable_t810x_snap_mode):?IISDNu8bit,
			(HW#hardware_data.clk_status):?IISDNu8bit,
			(HW#hardware_data.line_data)/binary, Csu/binary>>;
hardware_data(HW) when is_record(HW, hardware_data) ->
	LD = line_data(HW#hardware_data.line_data),
	NewHW = HW#hardware_data{line_data=LD},
	hardware_data(NewHW);
hardware_data(HW) when is_binary(HW) ->
	Size_line = (?IISDN_MAX_LINES * size(line_data(#line_data{}))),
	Size_csu = (?IISDN_MAX_LINES * size(<<0:?IISDNu8bit>>)),
	<<Clocking:?IISDNu8bit, Clocking2:?IISDNu8bit,
			Enable_clocking2:?IISDNu8bit, Netref_clocking:?IISDNu8bit,
			Netref_rate:?IISDNu8bit, Ctbus_mode:?IISDNu8bit,
			Force_framer_init:?IISDNu8bit, Tdm_rate:?IISDNu8bit,
			Enable_8370_rliu_monitor:?IISDNu8bit, Dbcount:?IISDNu8bit,
			Enable_t810x_snap_mode:?IISDNu8bit, Clk_status:?IISDNu8bit,
			LineData:Size_line/binary, Csu:Size_csu/binary>> = HW,
	U8toL = fun (Iter, <<>>, List) -> List;
			(Iter, <<Digit:?IISDNu8bit, Rest/binary>>, Acc) ->
				Iter(Iter, Rest, Acc ++ Digit)
			end,
	#hardware_data{clocking=Clocking, clocking2=Clocking2,
			enable_clocking2=Enable_clocking2, 
			netref_clocking=Netref_clocking, netref_rate=Netref_rate,
			ctbus_mode=Ctbus_mode, force_framer_init=Force_framer_init,
			tdm_rate=Tdm_rate,
			enable_8370_rliu_monitor=Enable_8370_rliu_monitor,
			dbcount=Dbcount, enable_t810x_snap_mode=Enable_t810x_snap_mode,
			clk_status=Clk_status, line_data=LineData, 
			csu = U8toL(U8toL, Csu, [])}.

line_data(LD) when is_record(LD, line_data) ->
	<<(LD#line_data.framing):?IISDNu8bit,
			(LD#line_data.line_code):?IISDNu8bit,
			(LD#line_data.pm_mode):?IISDNu8bit,
			(LD#line_data.line_length):?IISDNu8bit,
			(LD#line_data.term):?IISDNu8bit,
			(LD#line_data.line_type):?IISDNu8bit,
			(LD#line_data.integrate_alarms):?IISDNu8bit,
			(LD#line_data.filter_unsolicited):?IISDNu8bit,
			0:?IISDNu8bit,
			(LD#line_data.filter_yellow):?IISDNu8bit,
			(LD#line_data.bri_l1mode):?IISDNu8bit,
			(LD#line_data.briL1_cmd):?IISDNu8bit,
			(LD#line_data.bri_loop):?IISDNu8bit,
			(LD#line_data.bril1_t3):?IISDNu8bit,
			(LD#line_data.bril1_t4):?IISDNu16bit>>;
line_data(LD) when is_binary(LD) ->
	<<Framing:?IISDNu8bit, Line_code:?IISDNu8bit, Pm_mode:?IISDNu8bit,
			Line_length:?IISDNu8bit, Term:?IISDNu8bit, Line_type:?IISDNu8bit,
			Integrate_alarms:?IISDNu8bit, Filter_unsolicited:?IISDNu8bit,
			Filter_yellow:?IISDNu8bit, Bri_l1mode:?IISDNu8bit,
			BriL1_cmd:?IISDNu8bit, Bri_loop:?IISDNu8bit, 
			Bril1_t3:?IISDNu8bit, Bril1_t4:?IISDNu16bit>> = LD,
	#line_data{framing=Framing, line_code=Line_code,
			pm_mode=Pm_mode, line_length=Line_length, term=Term,
			line_type=Line_type, integrate_alarms=Integrate_alarms,
			filter_unsolicited=Filter_unsolicited, 
			filter_yellow=Filter_yellow, bri_l1mode=Bri_l1mode,
			briL1_cmd=BriL1_cmd, bri_loop=Bri_loop,
			bril1_t3=Bril1_t3, bril1_t4=Bril1_t4}.

tsi_data(TS) when is_record(TS, tsi_data),
		is_binary(TS#tsi_data.tsi_map) ->
	<<(TS#tsi_data.tsi_ack_enable):?IISDNu8bit,
			(TS#tsi_data.num_mappings):?IISDNu8bit,
			(TS#tsi_data.granularity):?IISDNu8bit,
			(TS#tsi_data.last):?IISDNu8bit,
			(TS#tsi_data.tsi_map)/binary>>;
tsi_data(TS) when is_record(TS, tsi_data) ->
	FunMap = fun(M, Bin) -> <<Bin/binary, (tsi_map(M))/binary>> end,
	MAP = lists:foldl(FunMap, <<>>, TS#tsi_data.tsi_map),
	tsi_data(TS#tsi_data{tsi_map = MAP}).

tsi_map(MAP) when is_record(MAP, tsi_map) ->
	<<(MAP#tsi_map.destination):?IISDNu16bit,
			(MAP#tsi_map.source):?IISDNu16bit>>;
tsi_map(MAP) when is_binary(MAP) ->
	<<Destination:?IISDNu16bit, Source:?IISDNu16bit>> = MAP,
	#tsi_map{destination = Destination, source = Source}.

q931_timers(T) when is_record(T, q931_timers) ->
	<<(T#q931_timers.t302):?IISDNu16bit,
			(T#q931_timers.t305):?IISDNu16bit,
			(T#q931_timers.t308):?IISDNu16bit,
			(T#q931_timers.t313):?IISDNu16bit,
			(T#q931_timers.t314):?IISDNu16bit,
			(T#q931_timers.t316):?IISDNu16bit,
			(T#q931_timers.t318):?IISDNu16bit,
			(T#q931_timers.t319):?IISDNu16bit,
			(T#q931_timers.t3m1):?IISDNu16bit,
			(T#q931_timers.t321):?IISDNu16bit>>.
