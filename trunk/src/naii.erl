%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% naii.erl   Erlang module to encode/decode naii structures/records   %%%
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
%%% For every structure definition in naii.h there should be here:      %%%
%%%                                                                     %%%
%%%        - a record definition (e.g. #'PRI_Q931_CNFG'{})              %%%
%%%                                                                     %%%
%%%        - a function (e.g. #'PRI_Q931_CNFG'(Q931) -> binary())       %%%
%%%                                                                     %%%
%%% A received binary is decoded to a record, or a record is encoded    %%%
%%% to a binary with the function of the same name as the structure     %%%
%%% using the record definition of the same name.                       %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(naii).

%%% TODO:  how do we group these?
-export(['L3_to_L4_struct'/1, 'L4_to_L3_struct'/1]).
-export(['PRI_ENA_PROTO_DATA'/1]).
-export(['PRI_HARDWARE_DATA'/1, 'PRI_LINE_DATA'/1]).
-export(['PRI_Q931_TIMERS'/1]).
-export(['PRI_DATA_INTERFACE_CONFIGURATION'/1]).
-export(['PRI_LEVEL1_CNFG'/1, 'PRI_LEVEL2_CNFG'/1, 'PRI_LEVEL3_CNFG'/1]).
-export(['PRI_L2_LAP_PARAMS'/1, 'PRI_L2_SS7_PARAMS'/1, 
		'PRI_L2_UDPIP_PARAMS'/1, 'PRI_L2_DPNSS_PARAMS'/1,
		'PRI_L2_V110_PARAMS'/1]).
-export(['PRI_DATA_INTERFACE'/1]).
-export(['PRI_Q931_CNFG'/1, 'PRI_BONDING_DATA'/1, 'PRI_X25_CONFIG'/1,
		'PRI_PM_CONFIG'/1, 'PRI_RELAY_CONFIG'/1, 'PRI_DPNSSCC_CONFIG'/1,
		'PRI_DASSCC_CONFIG'/1, 'PRI_Q933A_CONFIG'/1]).
-export(['PRI_L2_LAP_CONSTS'/1, 'PRI_L2_SS7_CONSTS'/1,
		'PRI_L2_IP_CONSTS'/1, 'PRI_L2_DPNSS_CONSTS'/1]).

-include("naii.hrl").


%%
%% L4_to_L3_struct
%%
%% This function takes a record of the same name and returns a binary
%% repsentaion of an entire SMI L4L3 message suitable for sending to
%% the boards.  If the data member is assigned a binary it will be sent
%% as is.  If it is undefined a default record of the type associated
%% with msgtype is used.  If data is a record of the appropriate type
%% it will be used to build the binary.
%%
'L4_to_L3_struct'(R) when is_record(R, 'L4_to_L3_struct') ->
	MessageType = R#'L4_to_L3_struct'.msgtype, 
	CommonHeader = <<(R#'L4_to_L3_struct'.lapdid):?PRIu8bit, 
			(R#'L4_to_L3_struct'.msgtype):?PRIu8bit,
			(R#'L4_to_L3_struct'.l4_ref):?PRIu16bit,
			(R#'L4_to_L3_struct'.call_ref):?PRIu16bit,
			(R#'L4_to_L3_struct'.lli):?PRIu16bit>>,
	MessageSpecificData = R#'L4_to_L3_struct'.data,
	'L4_to_L3_struct'(MessageType, CommonHeader, MessageSpecificData).
'L4_to_L3_struct'(_, Header, Data) when is_binary(Data) ->
	<<0, Header/binary, Data/binary>>;
'L4_to_L3_struct'(?L4L3mSET_HARDWARE, Header, Data) ->
	<<0, Header/binary, ('PRI_HARDWARE_DATA'(Data))/binary>>;
'L4_to_L3_struct'(?L4L3mSET_TSI, Header, Data) ->
	<<0, Header/binary, ('PRI_HARDWARE_DATA'(Data))/binary>>;
'L4_to_L3_struct'(?L3mENABLE_PROTOCOL, Header, Data) ->
	<<0, Header/binary, ('PRI_ENA_PROTO_DATA'(Data))/binary>>;
'L4_to_L3_struct'(?L4L3mREQ_HW_STATUS, Header, Data) ->
	<<0, Header/binary>>;
'L4_to_L3_struct'(?L4L3mREQ_TSI_STATUS, Header, Data) ->
	<<0, Header/binary>>.

'L3_to_L4_struct'(Bin) when is_binary(Bin) ->
	<<Lapdid:?PRIu8bit, Msgtype:?PRIu8bit, L4_ref:?PRIu16bit,
		Call_ref:?PRIu16bit, Bchanel:?PRIu8bit, Iface:?PRIu8bit,
		Bchannel_mask:?PRIu32bit, Lli:?PRIu16bit, Data_channel:?PRIu16bit,
		Data/binary>> = Bin,
	#'L3_to_L4_struct'{lapdid=Lapdid, msgtype=Msgtype, l4_ref=L4_ref,
			call_ref=Call_ref, bchanel=Bchanel, iface=Iface,
			bchannel_mask=Bchannel_mask, lli=Lli, 
			data_channel=Data_channel, data=Data}.



'PRI_LEVEL1_CNFG'(L1) when is_record(L1, 'PRI_LEVEL1_CNFG') ->
	<<(L1#'PRI_LEVEL1_CNFG'.l1_mode):?PRIu8bit,
			(L1#'PRI_LEVEL1_CNFG'.invert_hdlc):?PRIu8bit,
	      (L1#'PRI_LEVEL1_CNFG'.num_txbuf):?PRIu16bit,
	      (L1#'PRI_LEVEL1_CNFG'.num_rxbuf):?PRIu16bit,
	      (L1#'PRI_LEVEL1_CNFG'.buffsz):?PRIu16bit,
	      (L1#'PRI_LEVEL1_CNFG'.chain):?PRIu8bit,
	      (L1#'PRI_LEVEL1_CNFG'.device):?PRIu8bit,
	      (L1#'PRI_LEVEL1_CNFG'.bit_reverse):?PRIu8bit,
	      (L1#'PRI_LEVEL1_CNFG'.vme_lock):?PRIu8bit,
	      (L1#'PRI_LEVEL1_CNFG'.hdlc_channels):?PRIu32bit,
	      (L1#'PRI_LEVEL1_CNFG'.chan_kbit_rate):?PRIu16bit,
	      (L1#'PRI_LEVEL1_CNFG'.crc_bytes):?PRIu8bit,
	      (L1#'PRI_LEVEL1_CNFG'.crc_ignore_errs):?PRIu8bit,
			((L1#'PRI_LEVEL1_CNFG'.rate_adapt)#rate_adapt.enable):?PRIu8bit,
			((L1#'PRI_LEVEL1_CNFG'.rate_adapt)#rate_adapt.rate_adapt_value):?PRIu8bit,
			0:?PRIu16bit,
			((L1#'PRI_LEVEL1_CNFG'.raw_fillchar)#raw_fillchar.enable):?PRIu8bit,
			((L1#'PRI_LEVEL1_CNFG'.raw_fillchar)#raw_fillchar.fill_value):?PRIu8bit,
			0:?PRIu16bit,
			((L1#'PRI_LEVEL1_CNFG'.hdlc_flag_fill)#hdlc_flag_fill.enable):?PRIu8bit,
			((L1#'PRI_LEVEL1_CNFG'.hdlc_flag_fill)#hdlc_flag_fill.mode):?PRIu8bit,
			((L1#'PRI_LEVEL1_CNFG'.hdlc_flag_fill)#hdlc_flag_fill.value):?PRIu16bit,
			((L1#'PRI_LEVEL1_CNFG'.modem)#modem.originate):?PRIu8bit,
			((L1#'PRI_LEVEL1_CNFG'.modem)#modem.faxClass):?PRIu8bit,
			((L1#'PRI_LEVEL1_CNFG'.modem)#modem.encoding):?PRIu8bit,
			((L1#'PRI_LEVEL1_CNFG'.modem)#modem.amf):?PRIu8bit,
			(((L1#'PRI_LEVEL1_CNFG'.modem)#modem.amf_params)#amf_params.'0'):?PRIu32bit,
			(((L1#'PRI_LEVEL1_CNFG'.modem)#modem.amf_params)#amf_params.'1'):?PRIu32bit,
			(((L1#'PRI_LEVEL1_CNFG'.modem)#modem.amf_params)#amf_params.'2'):?PRIu32bit,
			(((L1#'PRI_LEVEL1_CNFG'.modem)#modem.amf_params)#amf_params.'3'):?PRIu32bit,
			((L1#'PRI_LEVEL1_CNFG'.modem)#modem.minBPS):?PRIu32bit,
			((L1#'PRI_LEVEL1_CNFG'.modem)#modem.maxBPS):?PRIu32bit,
			((L1#'PRI_LEVEL1_CNFG'.v110)#v110.bit_rate):?PRIu32bit,
			((L1#'PRI_LEVEL1_CNFG'.v110)#v110.auto_detect):?PRIu8bit,
			0:?PRIu8bit, 0:?PRIu8bit, 0:?PRIu8bit>>.
			

'PRI_LEVEL2_CNFG'(L2) when is_record(L2, 'PRI_LEVEL2_CNFG'),
		is_record(L2#'PRI_LEVEL2_CNFG'.par, 'PRI_L2_LAP_PARAMS') ->
	'PRI_LEVEL2_CNFG'(L2#'PRI_LEVEL2_CNFG'{
			par = 'PRI_L2_LAP_PARAMS'(L2#'PRI_LEVEL2_CNFG'.par)});
'PRI_LEVEL2_CNFG'(L2) when is_record(L2, 'PRI_LEVEL2_CNFG'),
		is_record(L2#'PRI_LEVEL2_CNFG'.par, 'PRI_L2_SS7_PARAMS') ->
	'PRI_LEVEL2_CNFG'(L2#'PRI_LEVEL2_CNFG'{
			par = 'PRI_L2_SS7_PARAMS'(L2#'PRI_LEVEL2_CNFG'.par)});
'PRI_LEVEL2_CNFG'(L2) when is_record(L2, 'PRI_LEVEL2_CNFG'),
		is_record(L2#'PRI_LEVEL2_CNFG'.par, 'PRI_L2_UDPIP_PARAMS') ->
	'PRI_LEVEL2_CNFG'(L2#'PRI_LEVEL2_CNFG'{
			par = 'PRI_L2_UDPIP_PARAMS'(L2#'PRI_LEVEL2_CNFG'.par)});
'PRI_LEVEL2_CNFG'(L2) when is_record(L2, 'PRI_LEVEL2_CNFG'),
		is_record(L2#'PRI_LEVEL2_CNFG'.par, 'PRI_L2_DPNSS_PARAMS') ->
	'PRI_LEVEL2_CNFG'(L2#'PRI_LEVEL2_CNFG'{
			par = 'PRI_L2_DPNSS_PARAMS'(L2#'PRI_LEVEL2_CNFG'.par)});
'PRI_LEVEL2_CNFG'(L2) when is_record(L2, 'PRI_LEVEL2_CNFG'),
		is_record(L2#'PRI_LEVEL2_CNFG'.par, 'PRI_L2_V110_PARAMS') ->
	'PRI_LEVEL2_CNFG'(L2#'PRI_LEVEL2_CNFG'{
			par = 'PRI_L2_V110_PARAMS'(L2#'PRI_LEVEL2_CNFG'.par)});
'PRI_LEVEL2_CNFG'(L2) when is_record(L2, 'PRI_LEVEL2_CNFG'),
		is_record(L2#'PRI_LEVEL2_CNFG'.data_interface, 'PRI_DATA_INTERFACE') ->
	'PRI_LEVEL2_CNFG'(L2#'PRI_LEVEL2_CNFG'{
			data_interface = 
					'PRI_DATA_INTERFACE'(L2#'PRI_LEVEL2_CNFG'.data_interface)});
'PRI_LEVEL2_CNFG'(L2) when is_record(L2, 'PRI_LEVEL2_CNFG'),
		is_record(L2#'PRI_LEVEL2_CNFG'.consts, 'PRI_L2_LAP_CONSTS') ->
	'PRI_LEVEL2_CNFG'(L2#'PRI_LEVEL2_CNFG'{
			consts = 'PRI_L2_LAP_CONSTS'(L2#'PRI_LEVEL2_CNFG'.consts)});
'PRI_LEVEL2_CNFG'(L2) when is_record(L2, 'PRI_LEVEL2_CNFG'),
		is_record(L2#'PRI_LEVEL2_CNFG'.consts, 'PRI_L2_SS7_CONSTS') ->
	'PRI_LEVEL2_CNFG'(L2#'PRI_LEVEL2_CNFG'{
			consts = 'PRI_L2_SS7_CONSTS'(L2#'PRI_LEVEL2_CNFG'.consts)});
'PRI_LEVEL2_CNFG'(L2) when is_record(L2, 'PRI_LEVEL2_CNFG'),
		is_record(L2#'PRI_LEVEL2_CNFG'.consts, 'PRI_L2_IP_CONSTS') ->
	'PRI_LEVEL2_CNFG'(L2#'PRI_LEVEL2_CNFG'{
			consts = 'PRI_L2_IP_CONSTS'(L2#'PRI_LEVEL2_CNFG'.consts)});
'PRI_LEVEL2_CNFG'(L2) when is_record(L2, 'PRI_LEVEL2_CNFG'),
		is_record(L2#'PRI_LEVEL2_CNFG'.consts, 'PRI_L2_DPNSS_CONSTS') ->
	'PRI_LEVEL2_CNFG'(L2#'PRI_LEVEL2_CNFG'{
			consts = 'PRI_L2_DPNSS_CONSTS'(L2#'PRI_LEVEL2_CNFG'.consts)});
'PRI_LEVEL2_CNFG'(L2) when is_record(L2, 'PRI_LEVEL2_CNFG'),
		is_binary(L2#'PRI_LEVEL2_CNFG'.par);
		is_binary(L2#'PRI_LEVEL2_CNFG'.data_interface);
		is_binary(L2#'PRI_LEVEL2_CNFG'.consts) ->
	<<(L2#'PRI_LEVEL2_CNFG'.par)/binary, 
			(L2#'PRI_LEVEL2_CNFG'.data_interface)/binary,
			(L2#'PRI_LEVEL2_CNFG'.consts)/binary>>.


'PRI_DATA_INTERFACE'(DataIf) when is_record(DataIf, 'PRI_DATA_INTERFACE') ->
	<<(DataIf#'PRI_DATA_INTERFACE'.enable):?PRIu8bit,
			(DataIf#'PRI_DATA_INTERFACE'.data_channel):?PRIu8bit,
			(DataIf#'PRI_DATA_INTERFACE'.fillandspill):?PRIu8bit,
			(DataIf#'PRI_DATA_INTERFACE'.allow_buffer_preload):?PRIu8bit>>.

'PRI_L2_LAP_PARAMS'(Lap) when is_record(Lap, 'PRI_L2_LAP_PARAMS') ->
	<<(Lap#'PRI_L2_LAP_PARAMS'.mode):?PRIu8bit,
			(Lap#'PRI_L2_LAP_PARAMS'.dce_dte):?PRIu8bit,
			(Lap#'PRI_L2_LAP_PARAMS'.tei_mode):?PRIu8bit,
			(Lap#'PRI_L2_LAP_PARAMS'.no_sabme):?PRIu8bit,
			(Lap#'PRI_L2_LAP_PARAMS'.l2_detail):?PRIu8bit,
			(Lap#'PRI_L2_LAP_PARAMS'.timestamp):?PRIu8bit,
			(Lap#'PRI_L2_LAP_PARAMS'.ui_mode):?PRIu8bit,
			(Lap#'PRI_L2_LAP_PARAMS'.priority):?PRIu8bit,
			(Lap#'PRI_L2_LAP_PARAMS'.no_reestab):?PRIu8bit,
			(Lap#'PRI_L2_LAP_PARAMS'.mode_1tr6):?PRIu8bit,
			(Lap#'PRI_L2_LAP_PARAMS'.mode_tei_1):?PRIu8bit,
			(Lap#'PRI_L2_LAP_PARAMS'.no_piggyback):?PRIu8bit>>.

'PRI_L2_SS7_PARAMS'(Mtp2) when is_record(Mtp2, 'PRI_L2_SS7_PARAMS') ->
	<<(Mtp2#'PRI_L2_SS7_PARAMS'.mode):?PRIu8bit,
			(Mtp2#'PRI_L2_SS7_PARAMS'.variant):?PRIu8bit,
			0:?PRIu16bit,
			0:?PRIu8bit, 0:?PRIu8bit, 0:?PRIu8bit, 0:?PRIu8bit,
			0:?PRIu8bit, 0:?PRIu8bit, 0:?PRIu8bit, 0:?PRIu8bit>>.

'PRI_L2_UDPIP_PARAMS'(Udpip) when is_record(Udpip, 'PRI_L2_UDPIP_PARAMS') ->
	<<(Udpip#'PRI_L2_UDPIP_PARAMS'.mode):?PRIu8bit,
			0:?PRIu8bit,
			(Udpip#'PRI_L2_UDPIP_PARAMS'.dstport):?PRIu16bit,
			(Udpip#'PRI_L2_UDPIP_PARAMS'.dstipaddr):?PRIu32bit,
			0:?PRIu8bit, 0:?PRIu8bit, 0:?PRIu8bit, 0:?PRIu8bit>>.

'PRI_L2_DPNSS_PARAMS'(Dpnss) when is_record(Dpnss, 'PRI_L2_DPNSS_PARAMS') ->
	<<(Dpnss#'PRI_L2_DPNSS_PARAMS'.mode):?PRIu8bit,
			(Dpnss#'PRI_L2_DPNSS_PARAMS'.pbx_b):?PRIu8bit,
			(Dpnss#'PRI_L2_DPNSS_PARAMS'.sabmr_as_ack):?PRIu8bit,
			(Dpnss#'PRI_L2_DPNSS_PARAMS'.tie_line_mode):?PRIu8bit>>.

'PRI_L2_V110_PARAMS'(V110) when is_record(V110, 'PRI_L2_V110_PARAMS') ->
	<<(V110#'PRI_L2_V110_PARAMS'.mode):?PRIu8bit,
			(V110#'PRI_L2_V110_PARAMS'.ebits):?PRIu8bit,
			(V110#'PRI_L2_V110_PARAMS'.flow_control):?PRIu8bit,
			(V110#'PRI_L2_V110_PARAMS'.nine_byte_rx_frames):?PRIu8bit,
			(V110#'PRI_L2_V110_PARAMS'.num_tx_idle_frames):?PRIu16bit,
			(V110#'PRI_L2_V110_PARAMS'.max_rx_frame_size):?PRIu16bit,
			(V110#'PRI_L2_V110_PARAMS'.stale_rx_data_timer):?PRIu16bit,
			(V110#'PRI_L2_V110_PARAMS'.filter_status_messages):?PRIu8bit,
			0:?PRIu8bit>>.
	
'PRI_L2_LAP_CONSTS'(L2) when is_record(L2, 'PRI_L2_LAP_CONSTS') ->
	<<(L2#'PRI_L2_LAP_CONSTS'.t200):?PRIu16bit,
			(L2#'PRI_L2_LAP_CONSTS'.t201):?PRIu16bit,
			(L2#'PRI_L2_LAP_CONSTS'.t202):?PRIu16bit,
			(L2#'PRI_L2_LAP_CONSTS'.t203):?PRIu16bit,
			(L2#'PRI_L2_LAP_CONSTS'.n200):?PRIu16bit,
			(L2#'PRI_L2_LAP_CONSTS'.n201):?PRIu16bit,
			(L2#'PRI_L2_LAP_CONSTS'.n202):?PRIu16bit,
			(L2#'PRI_L2_LAP_CONSTS'.k):?PRIu16bit>>.

'PRI_L2_SS7_CONSTS'(Mtp2) when is_record(Mtp2, 'PRI_L2_SS7_CONSTS') ->
	<<(Mtp2#'PRI_L2_SS7_CONSTS'.t1):?PRIu16bit,
			(Mtp2#'PRI_L2_SS7_CONSTS'.t2):?PRIu16bit,
			(Mtp2#'PRI_L2_SS7_CONSTS'.t3):?PRIu16bit,
			(Mtp2#'PRI_L2_SS7_CONSTS'.t4n):?PRIu16bit,
			(Mtp2#'PRI_L2_SS7_CONSTS'.t4e):?PRIu16bit,
			(Mtp2#'PRI_L2_SS7_CONSTS'.t5):?PRIu16bit,
			(Mtp2#'PRI_L2_SS7_CONSTS'.t6):?PRIu16bit,
			(Mtp2#'PRI_L2_SS7_CONSTS'.t7):?PRIu16bit>>.

'PRI_L2_IP_CONSTS'(Ip) when is_record(Ip, 'PRI_L2_IP_CONSTS') ->
	<<(Ip#'PRI_L2_IP_CONSTS'.no_dhcp):?PRIu8bit,
			0:?PRIu8bit, 0:?PRIu8bit, 0:?PRIu8bit,
			(Ip#'PRI_L2_IP_CONSTS'.ipaddr):?PRIu32bit,
			(Ip#'PRI_L2_IP_CONSTS'.gwaddr):?PRIu32bit,
			(Ip#'PRI_L2_IP_CONSTS'.subnet_mask):?PRIu32bit>>.

'PRI_L2_DPNSS_CONSTS'(Dpnss) when is_record(Dpnss, 'PRI_L2_DPNSS_CONSTS') ->
	<<(Dpnss#'PRI_L2_DPNSS_CONSTS'.nl):?PRIu16bit,
			0:?PRIu16bit,
			(Dpnss#'PRI_L2_DPNSS_CONSTS'.nt1):?PRIu32bit,
			(Dpnss#'PRI_L2_DPNSS_CONSTS'.nt2):?PRIu32bit>>.



'PRI_LEVEL3_CNFG'(L3) when is_record(L3, 'PRI_LEVEL3_CNFG') ->
	Mode = L3#'PRI_LEVEL3_CNFG'.l3_mode,
	if
		Mode == ?PRIl3modQ931 ->
			Cnfg = 'PRI_Q931_CNFG'(L3#'PRI_LEVEL3_CNFG'.cnfg);
		Mode == ?PRIl3modBONDING ->
			Cnfg = 'PRI_BONDING_DATA'(L3#'PRI_LEVEL3_CNFG'.cnfg);
		Mode == ?PRIl3modX25_PKT ->
			Cnfg = 'PRI_X25_CONFIG'(L3#'PRI_LEVEL3_CNFG'.cnfg);
		Mode == ?PRIl3modPM ->
			Cnfg = 'PRI_PM_CONFIG'(L3#'PRI_LEVEL3_CNFG'.cnfg);
		Mode == ?PRIl3modRELAY ->
			Cnfg = 'PRI_RELAY_CONFIG'(L3#'PRI_LEVEL3_CNFG'.cnfg);
		Mode == ?PRIl3modDPNSS ->
			Cnfg = 'PRI_DPNSSCC_CONFIG'(L3#'PRI_LEVEL3_CNFG'.cnfg);
		Mode == ?PRIl3modDASS ->
			Cnfg = 'PRI_DASSCC_CONFIG'(L3#'PRI_LEVEL3_CNFG'.cnfg);
		Mode == ?PRIl3modQ933_ANNEX_A ->
			Cnfg = 'PRI_Q933A_CONFIG'(L3#'PRI_LEVEL3_CNFG'.cnfg)
	end,
	<<Mode:?PRIu8bit, 0:?PRIu8bit, 0:?PRIu16bit, Cnfg/binary>>.

'PRI_Q931_CNFG'(Q931) when is_record(Q931, 'PRI_Q931_CNFG') ->
	Q931_Timers = 'PRI_Q931_TIMERS'(Q931#'PRI_Q931_CNFG'.q931_timers),
	Digit32 = fun(Digit, Bin) -> <<Digit:?PRIu32bit, Bin/binary>> end,
	B_channel_service_state = lists:foldr(Digit32, <<>>,
			Q931#'PRI_Q931_CNFG'.b_channel_service_state),
	Digit8 = fun(Digit, Bin) -> <<Digit:?PRIu8bit, Bin/binary>> end,
	Spid = lists:foldr(Digit8, <<>>, Q931#'PRI_Q931_CNFG'.spid),
	Spid_1 = lists:foldr(Digit8, <<>>, Q931#'PRI_Q931_CNFG'.spid_1),
	Dn = lists:foldr(Digit8, <<>>, Q931#'PRI_Q931_CNFG'.dn),
	Dn_1 = lists:foldr(Digit8, <<>>, Q931#'PRI_Q931_CNFG'.dn_1),
	<<(Q931#'PRI_Q931_CNFG'.switch_type):?PRIu16bit,
			(Q931#'PRI_Q931_CNFG'.variant):?PRIu16bit,
			(Q931#'PRI_Q931_CNFG'.call_filtering):?PRIu32bit,
			B_channel_service_state/binary,
			(Q931#'PRI_Q931_CNFG'.nfas):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.e1_30_bchan):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.basic_rate):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.net_side_emul):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.b_chan_negot):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.proc_on_exclusv):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.chanid_slot_map):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.sprs_chanid_callproc):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.no_chanid_callproc):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.append_raw_qmsg):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.ccitt_mode):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.raw_qmsg):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.no_ie_errcheck):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.user_ie_encode):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.overlap_rcv):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.send_l3l4_callproc):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.sending_cmplt):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.require_send_complete):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.report_incoming_callproc):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.no_tx_conn_ack):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.no_rx_conn_ack):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.sprs_chanid_setupack):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.no_chanid_setupack):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.no_canned_spid_rej):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.call_reject_notify):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.advice_of_charge):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.message_segmentation):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.no_bc_user_info):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.incoming_call_slot_map):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.release_complete_control):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.primary_lapdid):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.backup_lapdid):?PRIu8bit,
			0:?PRIu16bit,
			(Q931#'PRI_Q931_CNFG'.primary_ifnum):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.backup_ifnum):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.backup_control):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.spid_len):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.spid_1_len):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.dn_len):?PRIu8bit,
			Spid/binary, Spid_1/binary, Dn/binary, Dn_1/binary,
			(Q931#'PRI_Q931_CNFG'.chan_id_high_bit):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.att_cust_bri_ekts):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.subscribe_connack):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.suppress_auto_spid):?PRIu8bit,
			(Q931#'PRI_Q931_CNFG'.accept_all_bri_calls):?PRIu8bit,
			0:?PRIu16bit>>.

'PRI_BONDING_DATA'(Bond) when is_record(Bond, 'PRI_BONDING_DATA') ->
	Digit32 = fun(Digit, Bin) -> <<Digit:?PRIu32bit, Bin/binary>> end,
	Directory = lists:foldr(Digit32, <<>>, 
			Bond#'PRI_BONDING_DATA'.directory),
	<<(Bond#'PRI_BONDING_DATA'.mode):?PRIu16bit,
			(Bond#'PRI_BONDING_DATA'.destination):?PRIu8bit,
			(Bond#'PRI_BONDING_DATA'.num_tx_buf):?PRIu8bit,
			(Bond#'PRI_BONDING_DATA'.num_rx_buf):?PRIu8bit,
			(Bond#'PRI_BONDING_DATA'.data_channel):?PRIu8bit,
			(Bond#'PRI_BONDING_DATA'.txinit):?PRIu16bit,
			(Bond#'PRI_BONDING_DATA'.txadd01):?PRIu16bit,
			(Bond#'PRI_BONDING_DATA'.txfa):?PRIu16bit,
			(Bond#'PRI_BONDING_DATA'.txdisc):?PRIu16bit,
			(Bond#'PRI_BONDING_DATA'.txdeq):?PRIu16bit,
			(Bond#'PRI_BONDING_DATA'.tcid):?PRIu16bit,
			(Bond#'PRI_BONDING_DATA'.tanull):?PRIu16bit,
			(Bond#'PRI_BONDING_DATA'.channels):?PRIu16bit,
			Directory/binary>>.
	
'PRI_X25_CONFIG'(X25) when is_record(X25, 'PRI_X25_CONFIG') ->
	<<(X25#'PRI_X25_CONFIG'.cfg_msk):?PRIu32bit,
			(X25#'PRI_X25_CONFIG'.t10):?PRIu16bit,
			(X25#'PRI_X25_CONFIG'.t11):?PRIu16bit,
			(X25#'PRI_X25_CONFIG'.t12):?PRIu16bit,
			(X25#'PRI_X25_CONFIG'.t13):?PRIu16bit,
			(X25#'PRI_X25_CONFIG'.t28):?PRIu16bit,
			(X25#'PRI_X25_CONFIG'.p):?PRIu16bit,
			(X25#'PRI_X25_CONFIG'.w):?PRIu8bit,
			(X25#'PRI_X25_CONFIG'.max_clr_retry):?PRIu8bit,
			(X25#'PRI_X25_CONFIG'.max_svcs):?PRIu8bit,
			(X25#'PRI_X25_CONFIG'.max_pvcs):?PRIu8bit>>.

'PRI_PM_CONFIG'(PM) when is_record(PM, 'PRI_PM_CONFIG') ->
	Digit8 = fun(Digit, Bin) -> <<Digit:?PRIu8bit, Bin/binary>> end,
	Equipmentid = lists:foldr(Digit8, <<>>, PM#'PRI_PM_CONFIG'.equipmentid),
	Locationid = lists:foldr(Digit8, <<>>, PM#'PRI_PM_CONFIG'.locationid),
	Frameid = lists:foldr(Digit8, <<>>, PM#'PRI_PM_CONFIG'.frameid),
	Unitid = lists:foldr(Digit8, <<>>, PM#'PRI_PM_CONFIG'.unitid),
	Facilityid = lists:foldr(Digit8, <<>>, PM#'PRI_PM_CONFIG'.facilityid),
	<<(PM#'PRI_PM_CONFIG'.mode):?PRIu8bit, 
			(PM#'PRI_PM_CONFIG'.carrier):?PRIu8bit,
			(PM#'PRI_PM_CONFIG'.fdl_alert):?PRIu8bit,
			0:?PRIu8bit, 0:?PRIu8bit,
			Equipmentid/binary, Locationid/binary, Frameid/binary,
			Unitid/binary, Facilityid/binary>>.

'PRI_RELAY_CONFIG'(Relay) when is_record(Relay, 'PRI_RELAY_CONFIG') ->
	<<(Relay#'PRI_RELAY_CONFIG'.default_dest):?PRIu8bit,
			(Relay#'PRI_RELAY_CONFIG'.default_dest_id):?PRIu8bit,
			(Relay#'PRI_RELAY_CONFIG'.default_root_idx):?PRIu8bit,
			0:?PRIu8bit, 0:?PRIu8bit>>.

'PRI_DPNSSCC_CONFIG'(Dpnss) when is_record(Dpnss, 'PRI_DPNSSCC_CONFIG') ->
	<<(Dpnss#'PRI_DPNSSCC_CONFIG'.pbx_y):?PRIs8bit,
			(Dpnss#'PRI_DPNSSCC_CONFIG'.no_virtual_channels):?PRIs8bit,
			(Dpnss#'PRI_DPNSSCC_CONFIG'.dest_addr_len):?PRIs8bit,
			0:?PRIu8bit,
			(Dpnss#'PRI_DPNSSCC_CONFIG'.b_channel_service_state):?PRIu32bit,
			(Dpnss#'PRI_DPNSSCC_CONFIG'.v_channel_service_state):?PRIu32bit,
			(Dpnss#'PRI_DPNSSCC_CONFIG'.t_i_msg):?PRIs32bit,
			(Dpnss#'PRI_DPNSSCC_CONFIG'.t_guard):?PRIs32bit>>.

'PRI_DASSCC_CONFIG'(Dass) when is_record(Dass, 'PRI_DASSCC_CONFIG') ->
	<<(Dass#'PRI_DASSCC_CONFIG'.b_channel_service_state):?PRIu32bit,
			(Dass#'PRI_DASSCC_CONFIG'.t_digit_racking):?PRIs32bit,
			(Dass#'PRI_DASSCC_CONFIG'.n_clear_retries):?PRIs8bit,
			0:?PRIu8bit, 0:?PRIu8bit, 0:?PRIu8bit>>.

'PRI_Q933A_CONFIG'(Q933a) when is_record(Q933a, 'PRI_Q933A_CONFIG') ->
	<<(Q933a#'PRI_Q933A_CONFIG'.network_side):?PRIu8bit,
			(Q933a#'PRI_Q933A_CONFIG'.n391):?PRIu8bit,
			(Q933a#'PRI_Q933A_CONFIG'.n392):?PRIu8bit,
			(Q933a#'PRI_Q933A_CONFIG'.n393):?PRIu8bit,
			(Q933a#'PRI_Q933A_CONFIG'.t391):?PRIu16bit,
			(Q933a#'PRI_Q933A_CONFIG'.t392):?PRIu16bit>>.


'PRI_DATA_INTERFACE_CONFIGURATION'(D) 
			when is_record(D, 'PRI_DATA_INTERFACE_CONFIGURATION') ->
	<<(D#'PRI_DATA_INTERFACE_CONFIGURATION'.dchan_descr_addr):?PRIp32bit,
			(D#'PRI_DATA_INTERFACE_CONFIGURATION'.num_dchan_descr):?PRIu16bit,
			0:?PRIu16bit,
			(D#'PRI_DATA_INTERFACE_CONFIGURATION'.dchan_event_queue_addr):?PRIp16bit,
			(D#'PRI_DATA_INTERFACE_CONFIGURATION'.num_l3l4_dchan_events):?PRIu16bit,
			(D#'PRI_DATA_INTERFACE_CONFIGURATION'.num_l4l3_dchan_events):?PRIu16bit>>;
'PRI_DATA_INTERFACE_CONFIGURATION'(DataIf) when is_binary(DataIf) ->
	<<Dchan_descr_addr:?PRIp32bit, Num_dchan_descr:?PRIu16bit,
			_:?PRIu16bit,
			Dchan_event_queue_addr:?PRIp16bit,
			Num_l3l4_dchan_events:?PRIu16bit,
			Num_l4l3_dchan_events:?PRIu16bit>> = DataIf,
	#'PRI_DATA_INTERFACE_CONFIGURATION'{dchan_descr_addr=Dchan_descr_addr,
			num_dchan_descr=Num_dchan_descr,
			dchan_event_queue_addr=Dchan_event_queue_addr,
			num_l3l4_dchan_events=Num_l3l4_dchan_events,
			num_l4l3_dchan_events=Num_l4l3_dchan_events}.

'PRI_ENA_PROTO_DATA'(Proto) when is_record(Proto, 'PRI_ENA_PROTO_DATA'),
		is_record(Proto#'PRI_ENA_PROTO_DATA'.level1, 'PRI_LEVEL1_CNFG') ->
	'PRI_ENA_PROTO_DATA'(Proto#'PRI_ENA_PROTO_DATA'{
			level1 = 'PRI_LEVEL1_CNFG'(Proto#'PRI_ENA_PROTO_DATA'.level1)});
'PRI_ENA_PROTO_DATA'(Proto) when is_record(Proto, 'PRI_ENA_PROTO_DATA'),
		is_record(Proto#'PRI_ENA_PROTO_DATA'.level2, 'PRI_LEVEL2_CNFG') ->
	'PRI_ENA_PROTO_DATA'(Proto#'PRI_ENA_PROTO_DATA'{
			level2 = 'PRI_LEVEL2_CNFG'(Proto#'PRI_ENA_PROTO_DATA'.level2)});
'PRI_ENA_PROTO_DATA'(Proto) when is_record(Proto, 'PRI_ENA_PROTO_DATA'),
		is_record(Proto#'PRI_ENA_PROTO_DATA'.level3, 'PRI_LEVEL3_CNFG') ->
	'PRI_ENA_PROTO_DATA'(Proto#'PRI_ENA_PROTO_DATA'{
			level3 = 'PRI_LEVEL3_CNFG'(Proto#'PRI_ENA_PROTO_DATA'.level3)});
'PRI_ENA_PROTO_DATA'(Proto) when is_record(Proto, 'PRI_ENA_PROTO_DATA'),
		is_binary(Proto#'PRI_ENA_PROTO_DATA'.level1);
		is_binary(Proto#'PRI_ENA_PROTO_DATA'.level2);
		is_binary(Proto#'PRI_ENA_PROTO_DATA'.level3) ->
	<<(Proto#'PRI_ENA_PROTO_DATA'.level1)/binary,
			(Proto#'PRI_ENA_PROTO_DATA'.level2)/binary,
			(Proto#'PRI_ENA_PROTO_DATA'.level3)/binary>>.

'PRI_LINE_DATA'(LD) when is_record(LD, 'PRI_LINE_DATA') ->
	<<(LD#'PRI_LINE_DATA'.framing):?PRIu8bit,
			(LD#'PRI_LINE_DATA'.line_code):?PRIu8bit,
			(LD#'PRI_LINE_DATA'.pm_mode):?PRIu8bit,
			(LD#'PRI_LINE_DATA'.line_length):?PRIu8bit,
			(LD#'PRI_LINE_DATA'.term):?PRIu8bit,
			(LD#'PRI_LINE_DATA'.line_type):?PRIu8bit,
			(LD#'PRI_LINE_DATA'.integrate_alarms):?PRIu8bit,
			(LD#'PRI_LINE_DATA'.filter_unsolicited):?PRIu8bit,
			0:?PRIu8bit,
			(LD#'PRI_LINE_DATA'.filter_yellow):?PRIu8bit,
			(LD#'PRI_LINE_DATA'.bri_l1mode):?PRIu8bit,
			(LD#'PRI_LINE_DATA'.briL1_cmd):?PRIu8bit,
			(LD#'PRI_LINE_DATA'.bri_loop):?PRIu8bit,
			(LD#'PRI_LINE_DATA'.bril1_t3):?PRIu8bit,
			(LD#'PRI_LINE_DATA'.bril1_t4):?PRIu16bit>>;
'PRI_LINE_DATA'(LD) when is_binary(LD) ->
	<<Framing:?PRIu8bit, Line_code:?PRIu8bit, Pm_mode:?PRIu8bit,
			Line_length:?PRIu8bit, Term:?PRIu8bit, Line_type:?PRIu8bit,
			Integrate_alarms:?PRIu8bit, Filter_unsolicited:?PRIu8bit,
			Filter_yellow:?PRIu8bit, Bri_l1mode:?PRIu8bit,
			BriL1_cmd:?PRIu8bit, Bri_loop:?PRIu8bit, 
			Bril1_t3:?PRIu8bit, Bril1_t4:?PRIu16bit>> = LD,
	#'PRI_LINE_DATA'{framing=Framing, line_code=Line_code,
			pm_mode=Pm_mode, line_length=Line_length, term=Term,
			line_type=Line_type, integrate_alarms=Integrate_alarms,
			filter_unsolicited=Filter_unsolicited, 
			filter_yellow=Filter_yellow, bri_l1mode=Bri_l1mode,
			briL1_cmd=BriL1_cmd, bri_loop=Bri_loop,
			bril1_t3=Bril1_t3, bril1_t4=Bril1_t4}.

'PRI_HARDWARE_DATA'(HW) when is_record(HW, 'PRI_HARDWARE_DATA'),
		is_binary(HW#'PRI_HARDWARE_DATA'.line_data) ->
	Digit8 = fun(Digit8, Bin) -> <<Digit8:?PRIu8bit, Bin/binary>> end,
	Csu = lists:foldr(Digit8, <<>>, HW#'PRI_HARDWARE_DATA'.csu),
	<<(HW#'PRI_HARDWARE_DATA'.clocking):?PRIu8bit,
			(HW#'PRI_HARDWARE_DATA'.clocking2):?PRIu8bit,
			(HW#'PRI_HARDWARE_DATA'.enable_clocking2):?PRIu8bit,
			(HW#'PRI_HARDWARE_DATA'.netref_clocking):?PRIu8bit,
			(HW#'PRI_HARDWARE_DATA'.netref_rate):?PRIu8bit,
			(HW#'PRI_HARDWARE_DATA'.ctbus_mode):?PRIu8bit,
			(HW#'PRI_HARDWARE_DATA'.force_framer_init):?PRIu8bit,
			(HW#'PRI_HARDWARE_DATA'.tdm_rate):?PRIu8bit,
			(HW#'PRI_HARDWARE_DATA'.enable_8370_rliu_monitor):?PRIu8bit,
			(HW#'PRI_HARDWARE_DATA'.dbcount):?PRIu8bit,
			(HW#'PRI_HARDWARE_DATA'.enable_t810x_snap_mode):?PRIu8bit,
			(HW#'PRI_HARDWARE_DATA'.clk_status):?PRIu8bit,
			(HW#'PRI_HARDWARE_DATA'.line_data)/binary, Csu/binary>>;
'PRI_HARDWARE_DATA'(HW) when is_record(HW, 'PRI_HARDWARE_DATA') ->
	LD = 'PRI_LINE_DATA'(HW#'PRI_HARDWARE_DATA'.line_data),
	NewHW = HW#'PRI_HARDWARE_DATA'{line_data=LD},
	'PRI_HARDWARE_DATA'(NewHW);
'PRI_HARDWARE_DATA'(HW) when is_binary(HW) ->
	Size_line = (?PRI_MAX_LINES * size('PRI_LINE_DATA'(undef))),
	Size_csu = (?PRI_MAX_LINES * size(<<0:?PRIu8bit>>)),
	<<Clocking:?PRIu8bit, Clocking2:?PRIu8bit,
			Enable_clocking2:?PRIu8bit, Netref_clocking:?PRIu8bit,
			Netref_rate:?PRIu8bit, Ctbus_mode:?PRIu8bit,
			Force_framer_init:?PRIu8bit, Tdm_rate:?PRIu8bit,
			Enable_8370_rliu_monitor:?PRIu8bit, Dbcount:?PRIu8bit,
			Enable_t810x_snap_mode:?PRIu8bit, Clk_status:?PRIu8bit,
			LineData:Size_line/binary, Csu:Size_csu/binary>> = HW,
	U8toL = fun (Iter, <<>>, List) -> List;
			(Iter, <<Digit:?PRIu8bit, Rest/binary>>, Acc) ->
				Iter(Iter, Rest, Acc ++ Digit)
			end,
	#'PRI_HARDWARE_DATA'{clocking=Clocking, clocking2=Clocking2,
			enable_clocking2=Enable_clocking2, 
			netref_clocking=Netref_clocking, netref_rate=Netref_rate,
			ctbus_mode=Ctbus_mode, force_framer_init=Force_framer_init,
			tdm_rate=Tdm_rate,
			enable_8370_rliu_monitor=Enable_8370_rliu_monitor,
			dbcount=Dbcount, enable_t810x_snap_mode=Enable_t810x_snap_mode,
			clk_status=Clk_status, line_data=LineData, 
			csu = U8toL(U8toL, Csu, [])}.

'PRI_Q931_TIMERS'(T) when is_record(T, 'PRI_Q931_TIMERS') ->
	<<(T#'PRI_Q931_TIMERS'.t302):?PRIu16bit,
			(T#'PRI_Q931_TIMERS'.t305):?PRIu16bit,
			(T#'PRI_Q931_TIMERS'.t308):?PRIu16bit,
			(T#'PRI_Q931_TIMERS'.t313):?PRIu16bit,
			(T#'PRI_Q931_TIMERS'.t314):?PRIu16bit,
			(T#'PRI_Q931_TIMERS'.t316):?PRIu16bit,
			(T#'PRI_Q931_TIMERS'.t318):?PRIu16bit,
			(T#'PRI_Q931_TIMERS'.t319):?PRIu16bit,
			(T#'PRI_Q931_TIMERS'.t3m1):?PRIu16bit,
			(T#'PRI_Q931_TIMERS'.t321):?PRIu16bit>>.
