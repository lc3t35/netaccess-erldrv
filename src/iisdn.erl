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
%%%        - a record definition (e.g. #'IISDN_Q931_CNFG'{})            %%%
%%%                                                                     %%%
%%%        - a function (e.g. #'IISDN_Q931_CNFG'(Q931) -> binary())     %%%
%%%                                                                     %%%
%%% A received binary is decoded to a record, or a record is encoded    %%%
%%% to a binary with the function of the same name as the structure     %%%
%%% using the record definition of the same name.                       %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(iisdn).

%%% TODO:  how do we group these?
-export(['L3_to_L4_struct'/1, 'L4_to_L3_struct'/1]).
-export(['IISDN_ENA_PROTO_DATA'/1]).
-export(['IISDN_HARDWARE_DATA'/1, 'IISDN_LINE_DATA'/1]).
-export(['IISDN_Q931_TIMERS'/1]).
-export(['IISDN_DATA_INTERFACE_CONFIGURATION'/1]).
-export(['IISDN_LEVEL1_CNFG'/1, 'IISDN_LEVEL2_CNFG'/1,
		'IISDN_LEVEL3_CNFG'/1]).
-export(['IISDN_L2_LAP_PARAMS'/1, 'IISDN_L2_SS7_PARAMS'/1, 
		'IISDN_L2_UDPIP_PARAMS'/1, 'IISDN_L2_DPNSS_PARAMS'/1,
		'IISDN_L2_V110_PARAMS'/1]).
-export(['IISDN_DATA_INTERFACE'/1]).
-export(['IISDN_Q931_CNFG'/1, 'IISDN_BONDING_DATA'/1,
		'IISDN_X25_CONFIG'/1, 'IISDN_PM_CONFIG'/1,
		'IISDN_RELAY_CONFIG'/1, 'IISDN_DPNSSCC_CONFIG'/1,
		'IISDN_DASSCC_CONFIG'/1, 'IISDN_Q933A_CONFIG'/1]).
-export(['IISDN_L2_LAP_CONSTS'/1, 'IISDN_L2_SS7_CONSTS'/1,
		'IISDN_L2_IP_CONSTS'/1, 'IISDN_L2_DPNSS_CONSTS'/1]).

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
'L4_to_L3_struct'(R) when is_record(R, 'L4_to_L3_struct') ->
	MessageType = R#'L4_to_L3_struct'.msgtype, 
	CommonHeader = <<(R#'L4_to_L3_struct'.lapdid):?IISDNu8bit, 
			(R#'L4_to_L3_struct'.msgtype):?IISDNu8bit,
			(R#'L4_to_L3_struct'.l4_ref):?IISDNu16bit,
			(R#'L4_to_L3_struct'.call_ref):?IISDNu16bit,
			(R#'L4_to_L3_struct'.lli):?IISDNu16bit>>,
	MessageSpecificData = R#'L4_to_L3_struct'.data,
	'L4_to_L3_struct'(MessageType, CommonHeader, MessageSpecificData).
'L4_to_L3_struct'(_, Header, Data) when is_binary(Data) ->
	<<0, Header/binary, Data/binary>>;
'L4_to_L3_struct'(?L4L3mSET_HARDWARE, Header, Data) ->
	<<0, Header/binary, ('IISDN_HARDWARE_DATA'(Data))/binary>>;
'L4_to_L3_struct'(?L4L3mSET_TSI, Header, Data) ->
	<<0, Header/binary, ('IISDN_HARDWARE_DATA'(Data))/binary>>;
'L4_to_L3_struct'(?L4L3mENABLE_PROTOCOL, Header, Data) ->
	<<0, Header/binary, ('IISDN_ENA_PROTO_DATA'(Data))/binary>>;
'L4_to_L3_struct'(?L4L3mREQ_HW_STATUS, Header, Data) ->
	<<0, Header/binary>>;
'L4_to_L3_struct'(?L4L3mREQ_TSI_STATUS, Header, Data) ->
	<<0, Header/binary>>.

'L3_to_L4_struct'(Bin) when is_binary(Bin) ->
	<<Lapdid:?IISDNu8bit, Msgtype:?IISDNu8bit, L4_ref:?IISDNu16bit,
		Call_ref:?IISDNu16bit, Bchanel:?IISDNu8bit, Iface:?IISDNu8bit,
		Bchannel_mask:?IISDNu32bit, Lli:?IISDNu16bit, Data_channel:?IISDNu16bit,
		Data/binary>> = Bin,
	#'L3_to_L4_struct'{lapdid=Lapdid, msgtype=Msgtype, l4_ref=L4_ref,
			call_ref=Call_ref, bchanel=Bchanel, iface=Iface,
			bchannel_mask=Bchannel_mask, lli=Lli, 
			data_channel=Data_channel, data=Data}.



'IISDN_LEVEL1_CNFG'(L1) when is_record(L1, 'IISDN_LEVEL1_CNFG') ->
	<<(L1#'IISDN_LEVEL1_CNFG'.l1_mode):?IISDNu8bit,
			(L1#'IISDN_LEVEL1_CNFG'.invert_hdlc):?IISDNu8bit,
	      (L1#'IISDN_LEVEL1_CNFG'.num_txbuf):?IISDNu16bit,
	      (L1#'IISDN_LEVEL1_CNFG'.num_rxbuf):?IISDNu16bit,
	      (L1#'IISDN_LEVEL1_CNFG'.buffsz):?IISDNu16bit,
	      (L1#'IISDN_LEVEL1_CNFG'.chain):?IISDNu8bit,
	      (L1#'IISDN_LEVEL1_CNFG'.device):?IISDNu8bit,
	      (L1#'IISDN_LEVEL1_CNFG'.bit_reverse):?IISDNu8bit,
	      (L1#'IISDN_LEVEL1_CNFG'.vme_lock):?IISDNu8bit,
	      (L1#'IISDN_LEVEL1_CNFG'.hdlc_channels):?IISDNu32bit,
	      (L1#'IISDN_LEVEL1_CNFG'.chan_kbit_rate):?IISDNu16bit,
	      (L1#'IISDN_LEVEL1_CNFG'.crc_bytes):?IISDNu8bit,
	      (L1#'IISDN_LEVEL1_CNFG'.crc_ignore_errs):?IISDNu8bit,
			((L1#'IISDN_LEVEL1_CNFG'.rate_adapt)#rate_adapt.enable):?IISDNu8bit,
			((L1#'IISDN_LEVEL1_CNFG'.rate_adapt)#rate_adapt.rate_adapt_value):?IISDNu8bit,
			0:?IISDNu16bit,
			((L1#'IISDN_LEVEL1_CNFG'.raw_fillchar)#raw_fillchar.enable):?IISDNu8bit,
			((L1#'IISDN_LEVEL1_CNFG'.raw_fillchar)#raw_fillchar.fill_value):?IISDNu8bit,
			0:?IISDNu16bit,
			((L1#'IISDN_LEVEL1_CNFG'.hdlc_flag_fill)#hdlc_flag_fill.enable):?IISDNu8bit,
			((L1#'IISDN_LEVEL1_CNFG'.hdlc_flag_fill)#hdlc_flag_fill.mode):?IISDNu8bit,
			((L1#'IISDN_LEVEL1_CNFG'.hdlc_flag_fill)#hdlc_flag_fill.value):?IISDNu16bit,
			((L1#'IISDN_LEVEL1_CNFG'.modem)#modem.originate):?IISDNu8bit,
			((L1#'IISDN_LEVEL1_CNFG'.modem)#modem.faxClass):?IISDNu8bit,
			((L1#'IISDN_LEVEL1_CNFG'.modem)#modem.encoding):?IISDNu8bit,
			((L1#'IISDN_LEVEL1_CNFG'.modem)#modem.amf):?IISDNu8bit,
			(((L1#'IISDN_LEVEL1_CNFG'.modem)#modem.amf_params)#amf_params.'0'):?IISDNu32bit,
			(((L1#'IISDN_LEVEL1_CNFG'.modem)#modem.amf_params)#amf_params.'1'):?IISDNu32bit,
			(((L1#'IISDN_LEVEL1_CNFG'.modem)#modem.amf_params)#amf_params.'2'):?IISDNu32bit,
			(((L1#'IISDN_LEVEL1_CNFG'.modem)#modem.amf_params)#amf_params.'3'):?IISDNu32bit,
			((L1#'IISDN_LEVEL1_CNFG'.modem)#modem.minBPS):?IISDNu32bit,
			((L1#'IISDN_LEVEL1_CNFG'.modem)#modem.maxBPS):?IISDNu32bit,
			((L1#'IISDN_LEVEL1_CNFG'.v110)#v110.bit_rate):?IISDNu32bit,
			((L1#'IISDN_LEVEL1_CNFG'.v110)#v110.auto_detect):?IISDNu8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit>>.
			

'IISDN_LEVEL2_CNFG'(L2) when is_record(L2, 'IISDN_LEVEL2_CNFG'),
		is_record(L2#'IISDN_LEVEL2_CNFG'.par, 'IISDN_L2_LAP_PARAMS') ->
	'IISDN_LEVEL2_CNFG'(L2#'IISDN_LEVEL2_CNFG'{
			par = 'IISDN_L2_LAP_PARAMS'(L2#'IISDN_LEVEL2_CNFG'.par)});
'IISDN_LEVEL2_CNFG'(L2) when is_record(L2, 'IISDN_LEVEL2_CNFG'),
		is_record(L2#'IISDN_LEVEL2_CNFG'.par, 'IISDN_L2_SS7_PARAMS') ->
	'IISDN_LEVEL2_CNFG'(L2#'IISDN_LEVEL2_CNFG'{
			par = 'IISDN_L2_SS7_PARAMS'(L2#'IISDN_LEVEL2_CNFG'.par)});
'IISDN_LEVEL2_CNFG'(L2) when is_record(L2, 'IISDN_LEVEL2_CNFG'),
		is_record(L2#'IISDN_LEVEL2_CNFG'.par, 'IISDN_L2_UDPIP_PARAMS') ->
	'IISDN_LEVEL2_CNFG'(L2#'IISDN_LEVEL2_CNFG'{
			par = 'IISDN_L2_UDPIP_PARAMS'(L2#'IISDN_LEVEL2_CNFG'.par)});
'IISDN_LEVEL2_CNFG'(L2) when is_record(L2, 'IISDN_LEVEL2_CNFG'),
		is_record(L2#'IISDN_LEVEL2_CNFG'.par, 'IISDN_L2_DPNSS_PARAMS') ->
	'IISDN_LEVEL2_CNFG'(L2#'IISDN_LEVEL2_CNFG'{
			par = 'IISDN_L2_DPNSS_PARAMS'(L2#'IISDN_LEVEL2_CNFG'.par)});
'IISDN_LEVEL2_CNFG'(L2) when is_record(L2, 'IISDN_LEVEL2_CNFG'),
		is_record(L2#'IISDN_LEVEL2_CNFG'.par, 'IISDN_L2_V110_PARAMS') ->
	'IISDN_LEVEL2_CNFG'(L2#'IISDN_LEVEL2_CNFG'{
			par = 'IISDN_L2_V110_PARAMS'(L2#'IISDN_LEVEL2_CNFG'.par)});
'IISDN_LEVEL2_CNFG'(L2) when is_record(L2, 'IISDN_LEVEL2_CNFG'),
		is_record(L2#'IISDN_LEVEL2_CNFG'.data_interface, 'IISDN_DATA_INTERFACE') ->
	'IISDN_LEVEL2_CNFG'(L2#'IISDN_LEVEL2_CNFG'{
			data_interface = 
					'IISDN_DATA_INTERFACE'(L2#'IISDN_LEVEL2_CNFG'.data_interface)});
'IISDN_LEVEL2_CNFG'(L2) when is_record(L2, 'IISDN_LEVEL2_CNFG'),
		is_record(L2#'IISDN_LEVEL2_CNFG'.consts, 'IISDN_L2_LAP_CONSTS') ->
	'IISDN_LEVEL2_CNFG'(L2#'IISDN_LEVEL2_CNFG'{
			consts = 'IISDN_L2_LAP_CONSTS'(L2#'IISDN_LEVEL2_CNFG'.consts)});
'IISDN_LEVEL2_CNFG'(L2) when is_record(L2, 'IISDN_LEVEL2_CNFG'),
		is_record(L2#'IISDN_LEVEL2_CNFG'.consts, 'IISDN_L2_SS7_CONSTS') ->
	'IISDN_LEVEL2_CNFG'(L2#'IISDN_LEVEL2_CNFG'{
			consts = 'IISDN_L2_SS7_CONSTS'(L2#'IISDN_LEVEL2_CNFG'.consts)});
'IISDN_LEVEL2_CNFG'(L2) when is_record(L2, 'IISDN_LEVEL2_CNFG'),
		is_record(L2#'IISDN_LEVEL2_CNFG'.consts, 'IISDN_L2_IP_CONSTS') ->
	'IISDN_LEVEL2_CNFG'(L2#'IISDN_LEVEL2_CNFG'{
			consts = 'IISDN_L2_IP_CONSTS'(L2#'IISDN_LEVEL2_CNFG'.consts)});
'IISDN_LEVEL2_CNFG'(L2) when is_record(L2, 'IISDN_LEVEL2_CNFG'),
		is_record(L2#'IISDN_LEVEL2_CNFG'.consts, 'IISDN_L2_DPNSS_CONSTS') ->
	'IISDN_LEVEL2_CNFG'(L2#'IISDN_LEVEL2_CNFG'{
			consts = 'IISDN_L2_DPNSS_CONSTS'(L2#'IISDN_LEVEL2_CNFG'.consts)});
'IISDN_LEVEL2_CNFG'(L2) when is_record(L2, 'IISDN_LEVEL2_CNFG'),
		is_binary(L2#'IISDN_LEVEL2_CNFG'.par);
		is_binary(L2#'IISDN_LEVEL2_CNFG'.data_interface);
		is_binary(L2#'IISDN_LEVEL2_CNFG'.consts) ->
	<<(L2#'IISDN_LEVEL2_CNFG'.par)/binary, 
			(L2#'IISDN_LEVEL2_CNFG'.data_interface)/binary,
			(L2#'IISDN_LEVEL2_CNFG'.consts)/binary>>.


'IISDN_DATA_INTERFACE'(DataIf) when is_record(DataIf, 'IISDN_DATA_INTERFACE') ->
	<<(DataIf#'IISDN_DATA_INTERFACE'.enable):?IISDNu8bit,
			(DataIf#'IISDN_DATA_INTERFACE'.data_channel):?IISDNu8bit,
			(DataIf#'IISDN_DATA_INTERFACE'.fillandspill):?IISDNu8bit,
			(DataIf#'IISDN_DATA_INTERFACE'.allow_buffer_preload):?IISDNu8bit>>.

'IISDN_L2_LAP_PARAMS'(Lap) when is_record(Lap, 'IISDN_L2_LAP_PARAMS') ->
	<<(Lap#'IISDN_L2_LAP_PARAMS'.mode):?IISDNu8bit,
			(Lap#'IISDN_L2_LAP_PARAMS'.dce_dte):?IISDNu8bit,
			(Lap#'IISDN_L2_LAP_PARAMS'.tei_mode):?IISDNu8bit,
			(Lap#'IISDN_L2_LAP_PARAMS'.no_sabme):?IISDNu8bit,
			(Lap#'IISDN_L2_LAP_PARAMS'.l2_detail):?IISDNu8bit,
			(Lap#'IISDN_L2_LAP_PARAMS'.timestamp):?IISDNu8bit,
			(Lap#'IISDN_L2_LAP_PARAMS'.ui_mode):?IISDNu8bit,
			(Lap#'IISDN_L2_LAP_PARAMS'.priority):?IISDNu8bit,
			(Lap#'IISDN_L2_LAP_PARAMS'.no_reestab):?IISDNu8bit,
			(Lap#'IISDN_L2_LAP_PARAMS'.mode_1tr6):?IISDNu8bit,
			(Lap#'IISDN_L2_LAP_PARAMS'.mode_tei_1):?IISDNu8bit,
			(Lap#'IISDN_L2_LAP_PARAMS'.no_piggyback):?IISDNu8bit>>.

'IISDN_L2_SS7_PARAMS'(Mtp2) when is_record(Mtp2, 'IISDN_L2_SS7_PARAMS') ->
	<<(Mtp2#'IISDN_L2_SS7_PARAMS'.mode):?IISDNu8bit,
			(Mtp2#'IISDN_L2_SS7_PARAMS'.variant):?IISDNu8bit,
			0:?IISDNu16bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit>>.

'IISDN_L2_UDPIP_PARAMS'(Udpip) when is_record(Udpip, 'IISDN_L2_UDPIP_PARAMS') ->
	<<(Udpip#'IISDN_L2_UDPIP_PARAMS'.mode):?IISDNu8bit,
			0:?IISDNu8bit,
			(Udpip#'IISDN_L2_UDPIP_PARAMS'.dstport):?IISDNu16bit,
			(Udpip#'IISDN_L2_UDPIP_PARAMS'.dstipaddr):?IISDNu32bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit>>.

'IISDN_L2_DPNSS_PARAMS'(Dpnss) when is_record(Dpnss, 'IISDN_L2_DPNSS_PARAMS') ->
	<<(Dpnss#'IISDN_L2_DPNSS_PARAMS'.mode):?IISDNu8bit,
			(Dpnss#'IISDN_L2_DPNSS_PARAMS'.pbx_b):?IISDNu8bit,
			(Dpnss#'IISDN_L2_DPNSS_PARAMS'.sabmr_as_ack):?IISDNu8bit,
			(Dpnss#'IISDN_L2_DPNSS_PARAMS'.tie_line_mode):?IISDNu8bit>>.

'IISDN_L2_V110_PARAMS'(V110) when is_record(V110, 'IISDN_L2_V110_PARAMS') ->
	<<(V110#'IISDN_L2_V110_PARAMS'.mode):?IISDNu8bit,
			(V110#'IISDN_L2_V110_PARAMS'.ebits):?IISDNu8bit,
			(V110#'IISDN_L2_V110_PARAMS'.flow_control):?IISDNu8bit,
			(V110#'IISDN_L2_V110_PARAMS'.nine_byte_rx_frames):?IISDNu8bit,
			(V110#'IISDN_L2_V110_PARAMS'.num_tx_idle_frames):?IISDNu16bit,
			(V110#'IISDN_L2_V110_PARAMS'.max_rx_frame_size):?IISDNu16bit,
			(V110#'IISDN_L2_V110_PARAMS'.stale_rx_data_timer):?IISDNu16bit,
			(V110#'IISDN_L2_V110_PARAMS'.filter_status_messages):?IISDNu8bit,
			0:?IISDNu8bit>>.
	
'IISDN_L2_LAP_CONSTS'(L2) when is_record(L2, 'IISDN_L2_LAP_CONSTS') ->
	<<(L2#'IISDN_L2_LAP_CONSTS'.t200):?IISDNu16bit,
			(L2#'IISDN_L2_LAP_CONSTS'.t201):?IISDNu16bit,
			(L2#'IISDN_L2_LAP_CONSTS'.t202):?IISDNu16bit,
			(L2#'IISDN_L2_LAP_CONSTS'.t203):?IISDNu16bit,
			(L2#'IISDN_L2_LAP_CONSTS'.n200):?IISDNu16bit,
			(L2#'IISDN_L2_LAP_CONSTS'.n201):?IISDNu16bit,
			(L2#'IISDN_L2_LAP_CONSTS'.n202):?IISDNu16bit,
			(L2#'IISDN_L2_LAP_CONSTS'.k):?IISDNu16bit>>.

'IISDN_L2_SS7_CONSTS'(Mtp2) when is_record(Mtp2, 'IISDN_L2_SS7_CONSTS') ->
	<<(Mtp2#'IISDN_L2_SS7_CONSTS'.t1):?IISDNu16bit,
			(Mtp2#'IISDN_L2_SS7_CONSTS'.t2):?IISDNu16bit,
			(Mtp2#'IISDN_L2_SS7_CONSTS'.t3):?IISDNu16bit,
			(Mtp2#'IISDN_L2_SS7_CONSTS'.t4n):?IISDNu16bit,
			(Mtp2#'IISDN_L2_SS7_CONSTS'.t4e):?IISDNu16bit,
			(Mtp2#'IISDN_L2_SS7_CONSTS'.t5):?IISDNu16bit,
			(Mtp2#'IISDN_L2_SS7_CONSTS'.t6):?IISDNu16bit,
			(Mtp2#'IISDN_L2_SS7_CONSTS'.t7):?IISDNu16bit>>.

'IISDN_L2_IP_CONSTS'(Ip) when is_record(Ip, 'IISDN_L2_IP_CONSTS') ->
	<<(Ip#'IISDN_L2_IP_CONSTS'.no_dhcp):?IISDNu8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit,
			(Ip#'IISDN_L2_IP_CONSTS'.ipaddr):?IISDNu32bit,
			(Ip#'IISDN_L2_IP_CONSTS'.gwaddr):?IISDNu32bit,
			(Ip#'IISDN_L2_IP_CONSTS'.subnet_mask):?IISDNu32bit>>.

'IISDN_L2_DPNSS_CONSTS'(Dpnss) when is_record(Dpnss, 'IISDN_L2_DPNSS_CONSTS') ->
	<<(Dpnss#'IISDN_L2_DPNSS_CONSTS'.nl):?IISDNu16bit,
			0:?IISDNu16bit,
			(Dpnss#'IISDN_L2_DPNSS_CONSTS'.nt1):?IISDNu32bit,
			(Dpnss#'IISDN_L2_DPNSS_CONSTS'.nt2):?IISDNu32bit>>.



'IISDN_LEVEL3_CNFG'(L3) when is_record(L3, 'IISDN_LEVEL3_CNFG') ->
	Mode = L3#'IISDN_LEVEL3_CNFG'.l3_mode,
	if
		Mode == ?IISDNl3modQ931 ->
			Cnfg = 'IISDN_Q931_CNFG'(L3#'IISDN_LEVEL3_CNFG'.cnfg);
		Mode == ?IISDNl3modBONDING ->
			Cnfg = 'IISDN_BONDING_DATA'(L3#'IISDN_LEVEL3_CNFG'.cnfg);
		Mode == ?IISDNl3modX25_PKT ->
			Cnfg = 'IISDN_X25_CONFIG'(L3#'IISDN_LEVEL3_CNFG'.cnfg);
		Mode == ?IISDNl3modPM ->
			Cnfg = 'IISDN_PM_CONFIG'(L3#'IISDN_LEVEL3_CNFG'.cnfg);
		Mode == ?IISDNl3modRELAY ->
			Cnfg = 'IISDN_RELAY_CONFIG'(L3#'IISDN_LEVEL3_CNFG'.cnfg);
		Mode == ?IISDNl3modDPNSS ->
			Cnfg = 'IISDN_DPNSSCC_CONFIG'(L3#'IISDN_LEVEL3_CNFG'.cnfg);
		Mode == ?IISDNl3modDASS ->
			Cnfg = 'IISDN_DASSCC_CONFIG'(L3#'IISDN_LEVEL3_CNFG'.cnfg);
		Mode == ?IISDNl3modQ933_ANNEX_A ->
			Cnfg = 'IISDN_Q933A_CONFIG'(L3#'IISDN_LEVEL3_CNFG'.cnfg)
	end,
	<<Mode:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu16bit, Cnfg/binary>>.

'IISDN_Q931_CNFG'(Q931) when is_record(Q931, 'IISDN_Q931_CNFG') ->
	Q931_Timers = 'IISDN_Q931_TIMERS'(Q931#'IISDN_Q931_CNFG'.q931_timers),
	Digit32 = fun(Digit, Bin) -> <<Digit:?IISDNu32bit, Bin/binary>> end,
	B_channel_service_state = lists:foldr(Digit32, <<>>,
			Q931#'IISDN_Q931_CNFG'.b_channel_service_state),
	Digit8 = fun(Digit, Bin) -> <<Digit:?IISDNu8bit, Bin/binary>> end,
	Spid = lists:foldr(Digit8, <<>>, Q931#'IISDN_Q931_CNFG'.spid),
	Spid_1 = lists:foldr(Digit8, <<>>, Q931#'IISDN_Q931_CNFG'.spid_1),
	Dn = lists:foldr(Digit8, <<>>, Q931#'IISDN_Q931_CNFG'.dn),
	Dn_1 = lists:foldr(Digit8, <<>>, Q931#'IISDN_Q931_CNFG'.dn_1),
	<<(Q931#'IISDN_Q931_CNFG'.switch_type):?IISDNu16bit,
			(Q931#'IISDN_Q931_CNFG'.variant):?IISDNu16bit,
			(Q931#'IISDN_Q931_CNFG'.call_filtering):?IISDNu32bit,
			B_channel_service_state/binary,
			(Q931#'IISDN_Q931_CNFG'.nfas):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.e1_30_bchan):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.basic_rate):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.net_side_emul):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.b_chan_negot):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.proc_on_exclusv):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.chanid_slot_map):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.sprs_chanid_callproc):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.no_chanid_callproc):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.append_raw_qmsg):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.ccitt_mode):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.raw_qmsg):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.no_ie_errcheck):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.user_ie_encode):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.overlap_rcv):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.send_l3l4_callproc):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.sending_cmplt):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.require_send_complete):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.report_incoming_callproc):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.no_tx_conn_ack):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.no_rx_conn_ack):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.sprs_chanid_setupack):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.no_chanid_setupack):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.no_canned_spid_rej):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.call_reject_notify):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.advice_of_charge):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.message_segmentation):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.no_bc_user_info):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.incoming_call_slot_map):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.release_complete_control):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.primary_lapdid):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.backup_lapdid):?IISDNu8bit,
			0:?IISDNu16bit,
			(Q931#'IISDN_Q931_CNFG'.primary_ifnum):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.backup_ifnum):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.backup_control):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.spid_len):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.spid_1_len):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.dn_len):?IISDNu8bit,
			Spid/binary, Spid_1/binary, Dn/binary, Dn_1/binary,
			(Q931#'IISDN_Q931_CNFG'.chan_id_high_bit):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.att_cust_bri_ekts):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.subscribe_connack):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.suppress_auto_spid):?IISDNu8bit,
			(Q931#'IISDN_Q931_CNFG'.accept_all_bri_calls):?IISDNu8bit,
			0:?IISDNu16bit>>.

'IISDN_BONDING_DATA'(Bond) when is_record(Bond, 'IISDN_BONDING_DATA') ->
	Digit32 = fun(Digit, Bin) -> <<Digit:?IISDNu32bit, Bin/binary>> end,
	Directory = lists:foldr(Digit32, <<>>, 
			Bond#'IISDN_BONDING_DATA'.directory),
	<<(Bond#'IISDN_BONDING_DATA'.mode):?IISDNu16bit,
			(Bond#'IISDN_BONDING_DATA'.destination):?IISDNu8bit,
			(Bond#'IISDN_BONDING_DATA'.num_tx_buf):?IISDNu8bit,
			(Bond#'IISDN_BONDING_DATA'.num_rx_buf):?IISDNu8bit,
			(Bond#'IISDN_BONDING_DATA'.data_channel):?IISDNu8bit,
			(Bond#'IISDN_BONDING_DATA'.txinit):?IISDNu16bit,
			(Bond#'IISDN_BONDING_DATA'.txadd01):?IISDNu16bit,
			(Bond#'IISDN_BONDING_DATA'.txfa):?IISDNu16bit,
			(Bond#'IISDN_BONDING_DATA'.txdisc):?IISDNu16bit,
			(Bond#'IISDN_BONDING_DATA'.txdeq):?IISDNu16bit,
			(Bond#'IISDN_BONDING_DATA'.tcid):?IISDNu16bit,
			(Bond#'IISDN_BONDING_DATA'.tanull):?IISDNu16bit,
			(Bond#'IISDN_BONDING_DATA'.channels):?IISDNu16bit,
			Directory/binary>>.
	
'IISDN_X25_CONFIG'(X25) when is_record(X25, 'IISDN_X25_CONFIG') ->
	<<(X25#'IISDN_X25_CONFIG'.cfg_msk):?IISDNu32bit,
			(X25#'IISDN_X25_CONFIG'.t10):?IISDNu16bit,
			(X25#'IISDN_X25_CONFIG'.t11):?IISDNu16bit,
			(X25#'IISDN_X25_CONFIG'.t12):?IISDNu16bit,
			(X25#'IISDN_X25_CONFIG'.t13):?IISDNu16bit,
			(X25#'IISDN_X25_CONFIG'.t28):?IISDNu16bit,
			(X25#'IISDN_X25_CONFIG'.p):?IISDNu16bit,
			(X25#'IISDN_X25_CONFIG'.w):?IISDNu8bit,
			(X25#'IISDN_X25_CONFIG'.max_clr_retry):?IISDNu8bit,
			(X25#'IISDN_X25_CONFIG'.max_svcs):?IISDNu8bit,
			(X25#'IISDN_X25_CONFIG'.max_pvcs):?IISDNu8bit>>.

'IISDN_PM_CONFIG'(PM) when is_record(PM, 'IISDN_PM_CONFIG') ->
	Digit8 = fun(Digit, Bin) -> <<Digit:?IISDNu8bit, Bin/binary>> end,
	Equipmentid = lists:foldr(Digit8, <<>>, PM#'IISDN_PM_CONFIG'.equipmentid),
	Locationid = lists:foldr(Digit8, <<>>, PM#'IISDN_PM_CONFIG'.locationid),
	Frameid = lists:foldr(Digit8, <<>>, PM#'IISDN_PM_CONFIG'.frameid),
	Unitid = lists:foldr(Digit8, <<>>, PM#'IISDN_PM_CONFIG'.unitid),
	Facilityid = lists:foldr(Digit8, <<>>, PM#'IISDN_PM_CONFIG'.facilityid),
	<<(PM#'IISDN_PM_CONFIG'.mode):?IISDNu8bit, 
			(PM#'IISDN_PM_CONFIG'.carrier):?IISDNu8bit,
			(PM#'IISDN_PM_CONFIG'.fdl_alert):?IISDNu8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit,
			Equipmentid/binary, Locationid/binary, Frameid/binary,
			Unitid/binary, Facilityid/binary>>.

'IISDN_RELAY_CONFIG'(Relay) when is_record(Relay, 'IISDN_RELAY_CONFIG') ->
	<<(Relay#'IISDN_RELAY_CONFIG'.default_dest):?IISDNu8bit,
			(Relay#'IISDN_RELAY_CONFIG'.default_dest_id):?IISDNu8bit,
			(Relay#'IISDN_RELAY_CONFIG'.default_root_idx):?IISDNu8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit>>.

'IISDN_DPNSSCC_CONFIG'(Dpnss) when is_record(Dpnss, 'IISDN_DPNSSCC_CONFIG') ->
	<<(Dpnss#'IISDN_DPNSSCC_CONFIG'.pbx_y):?IISDNs8bit,
			(Dpnss#'IISDN_DPNSSCC_CONFIG'.no_virtual_channels):?IISDNs8bit,
			(Dpnss#'IISDN_DPNSSCC_CONFIG'.dest_addr_len):?IISDNs8bit,
			0:?IISDNu8bit,
			(Dpnss#'IISDN_DPNSSCC_CONFIG'.b_channel_service_state):?IISDNu32bit,
			(Dpnss#'IISDN_DPNSSCC_CONFIG'.v_channel_service_state):?IISDNu32bit,
			(Dpnss#'IISDN_DPNSSCC_CONFIG'.t_i_msg):?IISDNs32bit,
			(Dpnss#'IISDN_DPNSSCC_CONFIG'.t_guard):?IISDNs32bit>>.

'IISDN_DASSCC_CONFIG'(Dass) when is_record(Dass, 'IISDN_DASSCC_CONFIG') ->
	<<(Dass#'IISDN_DASSCC_CONFIG'.b_channel_service_state):?IISDNu32bit,
			(Dass#'IISDN_DASSCC_CONFIG'.t_digit_racking):?IISDNs32bit,
			(Dass#'IISDN_DASSCC_CONFIG'.n_clear_retries):?IISDNs8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit>>.

'IISDN_Q933A_CONFIG'(Q933a) when is_record(Q933a, 'IISDN_Q933A_CONFIG') ->
	<<(Q933a#'IISDN_Q933A_CONFIG'.network_side):?IISDNu8bit,
			(Q933a#'IISDN_Q933A_CONFIG'.n391):?IISDNu8bit,
			(Q933a#'IISDN_Q933A_CONFIG'.n392):?IISDNu8bit,
			(Q933a#'IISDN_Q933A_CONFIG'.n393):?IISDNu8bit,
			(Q933a#'IISDN_Q933A_CONFIG'.t391):?IISDNu16bit,
			(Q933a#'IISDN_Q933A_CONFIG'.t392):?IISDNu16bit>>.


'IISDN_DATA_INTERFACE_CONFIGURATION'(D) 
			when is_record(D, 'IISDN_DATA_INTERFACE_CONFIGURATION') ->
	<<(D#'IISDN_DATA_INTERFACE_CONFIGURATION'.dchan_descr_addr):?IISDNp32bit,
			(D#'IISDN_DATA_INTERFACE_CONFIGURATION'.num_dchan_descr):?IISDNu16bit,
			0:?IISDNu16bit,
			(D#'IISDN_DATA_INTERFACE_CONFIGURATION'.dchan_event_queue_addr):?IISDNp16bit,
			(D#'IISDN_DATA_INTERFACE_CONFIGURATION'.num_l3l4_dchan_events):?IISDNu16bit,
			(D#'IISDN_DATA_INTERFACE_CONFIGURATION'.num_l4l3_dchan_events):?IISDNu16bit>>;
'IISDN_DATA_INTERFACE_CONFIGURATION'(DataIf) when is_binary(DataIf) ->
	<<Dchan_descr_addr:?IISDNp32bit, Num_dchan_descr:?IISDNu16bit,
			_:?IISDNu16bit,
			Dchan_event_queue_addr:?IISDNp16bit,
			Num_l3l4_dchan_events:?IISDNu16bit,
			Num_l4l3_dchan_events:?IISDNu16bit>> = DataIf,
	#'IISDN_DATA_INTERFACE_CONFIGURATION'{dchan_descr_addr=Dchan_descr_addr,
			num_dchan_descr=Num_dchan_descr,
			dchan_event_queue_addr=Dchan_event_queue_addr,
			num_l3l4_dchan_events=Num_l3l4_dchan_events,
			num_l4l3_dchan_events=Num_l4l3_dchan_events}.

'IISDN_ENA_PROTO_DATA'(Proto) when is_record(Proto, 'IISDN_ENA_PROTO_DATA'),
		is_record(Proto#'IISDN_ENA_PROTO_DATA'.level1, 'IISDN_LEVEL1_CNFG') ->
	'IISDN_ENA_PROTO_DATA'(Proto#'IISDN_ENA_PROTO_DATA'{
			level1 = 'IISDN_LEVEL1_CNFG'(Proto#'IISDN_ENA_PROTO_DATA'.level1)});
'IISDN_ENA_PROTO_DATA'(Proto) when is_record(Proto, 'IISDN_ENA_PROTO_DATA'),
		is_record(Proto#'IISDN_ENA_PROTO_DATA'.level2, 'IISDN_LEVEL2_CNFG') ->
	'IISDN_ENA_PROTO_DATA'(Proto#'IISDN_ENA_PROTO_DATA'{
			level2 = 'IISDN_LEVEL2_CNFG'(Proto#'IISDN_ENA_PROTO_DATA'.level2)});
'IISDN_ENA_PROTO_DATA'(Proto) when is_record(Proto, 'IISDN_ENA_PROTO_DATA'),
		is_record(Proto#'IISDN_ENA_PROTO_DATA'.level3, 'IISDN_LEVEL3_CNFG') ->
	'IISDN_ENA_PROTO_DATA'(Proto#'IISDN_ENA_PROTO_DATA'{
			level3 = 'IISDN_LEVEL3_CNFG'(Proto#'IISDN_ENA_PROTO_DATA'.level3)});
'IISDN_ENA_PROTO_DATA'(Proto) when is_record(Proto, 'IISDN_ENA_PROTO_DATA'),
		is_binary(Proto#'IISDN_ENA_PROTO_DATA'.level1);
		is_binary(Proto#'IISDN_ENA_PROTO_DATA'.level2);
		is_binary(Proto#'IISDN_ENA_PROTO_DATA'.level3) ->
	<<(Proto#'IISDN_ENA_PROTO_DATA'.level1)/binary,
			(Proto#'IISDN_ENA_PROTO_DATA'.level2)/binary,
			(Proto#'IISDN_ENA_PROTO_DATA'.level3)/binary>>.

'IISDN_LINE_DATA'(LD) when is_record(LD, 'IISDN_LINE_DATA') ->
	<<(LD#'IISDN_LINE_DATA'.framing):?IISDNu8bit,
			(LD#'IISDN_LINE_DATA'.line_code):?IISDNu8bit,
			(LD#'IISDN_LINE_DATA'.pm_mode):?IISDNu8bit,
			(LD#'IISDN_LINE_DATA'.line_length):?IISDNu8bit,
			(LD#'IISDN_LINE_DATA'.term):?IISDNu8bit,
			(LD#'IISDN_LINE_DATA'.line_type):?IISDNu8bit,
			(LD#'IISDN_LINE_DATA'.integrate_alarms):?IISDNu8bit,
			(LD#'IISDN_LINE_DATA'.filter_unsolicited):?IISDNu8bit,
			0:?IISDNu8bit,
			(LD#'IISDN_LINE_DATA'.filter_yellow):?IISDNu8bit,
			(LD#'IISDN_LINE_DATA'.bri_l1mode):?IISDNu8bit,
			(LD#'IISDN_LINE_DATA'.briL1_cmd):?IISDNu8bit,
			(LD#'IISDN_LINE_DATA'.bri_loop):?IISDNu8bit,
			(LD#'IISDN_LINE_DATA'.bril1_t3):?IISDNu8bit,
			(LD#'IISDN_LINE_DATA'.bril1_t4):?IISDNu16bit>>;
'IISDN_LINE_DATA'(LD) when is_binary(LD) ->
	<<Framing:?IISDNu8bit, Line_code:?IISDNu8bit, Pm_mode:?IISDNu8bit,
			Line_length:?IISDNu8bit, Term:?IISDNu8bit, Line_type:?IISDNu8bit,
			Integrate_alarms:?IISDNu8bit, Filter_unsolicited:?IISDNu8bit,
			Filter_yellow:?IISDNu8bit, Bri_l1mode:?IISDNu8bit,
			BriL1_cmd:?IISDNu8bit, Bri_loop:?IISDNu8bit, 
			Bril1_t3:?IISDNu8bit, Bril1_t4:?IISDNu16bit>> = LD,
	#'IISDN_LINE_DATA'{framing=Framing, line_code=Line_code,
			pm_mode=Pm_mode, line_length=Line_length, term=Term,
			line_type=Line_type, integrate_alarms=Integrate_alarms,
			filter_unsolicited=Filter_unsolicited, 
			filter_yellow=Filter_yellow, bri_l1mode=Bri_l1mode,
			briL1_cmd=BriL1_cmd, bri_loop=Bri_loop,
			bril1_t3=Bril1_t3, bril1_t4=Bril1_t4}.

'IISDN_HARDWARE_DATA'(HW) when is_record(HW, 'IISDN_HARDWARE_DATA'),
		is_binary(HW#'IISDN_HARDWARE_DATA'.line_data) ->
	Digit8 = fun(Digit8, Bin) -> <<Digit8:?IISDNu8bit, Bin/binary>> end,
	Csu = lists:foldr(Digit8, <<>>, HW#'IISDN_HARDWARE_DATA'.csu),
	<<(HW#'IISDN_HARDWARE_DATA'.clocking):?IISDNu8bit,
			(HW#'IISDN_HARDWARE_DATA'.clocking2):?IISDNu8bit,
			(HW#'IISDN_HARDWARE_DATA'.enable_clocking2):?IISDNu8bit,
			(HW#'IISDN_HARDWARE_DATA'.netref_clocking):?IISDNu8bit,
			(HW#'IISDN_HARDWARE_DATA'.netref_rate):?IISDNu8bit,
			(HW#'IISDN_HARDWARE_DATA'.ctbus_mode):?IISDNu8bit,
			(HW#'IISDN_HARDWARE_DATA'.force_framer_init):?IISDNu8bit,
			(HW#'IISDN_HARDWARE_DATA'.tdm_rate):?IISDNu8bit,
			(HW#'IISDN_HARDWARE_DATA'.enable_8370_rliu_monitor):?IISDNu8bit,
			(HW#'IISDN_HARDWARE_DATA'.dbcount):?IISDNu8bit,
			(HW#'IISDN_HARDWARE_DATA'.enable_t810x_snap_mode):?IISDNu8bit,
			(HW#'IISDN_HARDWARE_DATA'.clk_status):?IISDNu8bit,
			(HW#'IISDN_HARDWARE_DATA'.line_data)/binary, Csu/binary>>;
'IISDN_HARDWARE_DATA'(HW) when is_record(HW, 'IISDN_HARDWARE_DATA') ->
	LD = 'IISDN_LINE_DATA'(HW#'IISDN_HARDWARE_DATA'.line_data),
	NewHW = HW#'IISDN_HARDWARE_DATA'{line_data=LD},
	'IISDN_HARDWARE_DATA'(NewHW);
'IISDN_HARDWARE_DATA'(HW) when is_binary(HW) ->
	Size_line = (?IISDN_MAX_LINES * size('IISDN_LINE_DATA'(#'IISDN_LINE_DATA'{}))),
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
	#'IISDN_HARDWARE_DATA'{clocking=Clocking, clocking2=Clocking2,
			enable_clocking2=Enable_clocking2, 
			netref_clocking=Netref_clocking, netref_rate=Netref_rate,
			ctbus_mode=Ctbus_mode, force_framer_init=Force_framer_init,
			tdm_rate=Tdm_rate,
			enable_8370_rliu_monitor=Enable_8370_rliu_monitor,
			dbcount=Dbcount, enable_t810x_snap_mode=Enable_t810x_snap_mode,
			clk_status=Clk_status, line_data=LineData, 
			csu = U8toL(U8toL, Csu, [])}.

'IISDN_Q931_TIMERS'(T) when is_record(T, 'IISDN_Q931_TIMERS') ->
	<<(T#'IISDN_Q931_TIMERS'.t302):?IISDNu16bit,
			(T#'IISDN_Q931_TIMERS'.t305):?IISDNu16bit,
			(T#'IISDN_Q931_TIMERS'.t308):?IISDNu16bit,
			(T#'IISDN_Q931_TIMERS'.t313):?IISDNu16bit,
			(T#'IISDN_Q931_TIMERS'.t314):?IISDNu16bit,
			(T#'IISDN_Q931_TIMERS'.t316):?IISDNu16bit,
			(T#'IISDN_Q931_TIMERS'.t318):?IISDNu16bit,
			(T#'IISDN_Q931_TIMERS'.t319):?IISDNu16bit,
			(T#'IISDN_Q931_TIMERS'.t3m1):?IISDNu16bit,
			(T#'IISDN_Q931_TIMERS'.t321):?IISDNu16bit>>.
