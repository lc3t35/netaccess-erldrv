%%% src/naii.hrl.  Generated from naii.hrl.in by configure.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% naii.hrl   Erlang header file defining the structures in naii.h     %%%
%%%                                                                     %%%
%%%             This file defines the SMI interface.                    %%%
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
%%%  The Netaccess driver API header file <naii.h> defines the format   %%%
%%%  of the binary interface to the embedded processor on the card.     %%%
%%%  When we receive an incoming SMI control message from the driver    %%%
%%%  we want to parse out all the elements of the message.  The         %%%
%%%  <naii.h> header file defines how this is done.                     %%%
%%%                                                                     %%%
%%%  The full message received is matched against the L3L4 SMI Common   %%%
%%%  message structure:                                                 %%%
%%%                                                                     %%%
%%%      <<CommonHeader/binary, MessageSpecificData/binary>>            %%%
%%%                                                                     %%%
%%%      [Note:  determining size is left out of current discussion]    %%%
%%%                                                                     %%%
%%%  CommonHeader is matched against the L3L4 Message Common Header:    %%%
%%%                                                                     %%%
%%%    <<Lapdid, Msgtype, L4_ref, Call_ref, Bchannel, Interface,        %%%
%%%                    Bchannel_mask, Lli, Data_channel>>               %%%
%%%                                                                     %%%
%%%  Inspecting Msgtype we discover the appropriate mask to use for     %%%
%%%  matching the MessageSpecificData.                                  %%%
%%%                                                                     %%%
%%%  The L3L4 Message Common Header is defined in the typedef for a     %%%
%%%  structure in <naii.h> named L3_to_L4_struct.  In this file the     %%%
%%%  function 'L3_to_L4_struct'/1 defines the unpacking of that         %%%
%%%  structure into a record of the same name.  Similiarly a record     %%%
%%%  of type #'L4_to_L3_struct'{} is passed to the function             %%%
%%%  'L4_to_L3_struct'/1 to create a binary suitable to be sent to      %%%
%%%  board.                                                             %%%
%%%                                                                     %%%
%%%  Most structures which appear in <naii.h> should have in this       %%%
%%%  file both a corresponding record definition and a function with    %%%
%%%  arity one which accepts a record or a binary and returns a         %%%
%%%  record or a binary.                                                %%%
%%%                                                                     %%%



%% basic types
-define(PRIu8bit,  1/native-unsigned-integer-unit:8).
-define(PRIs8bit,  1/native-signed-integer-unit:8).
-define(PRIu16bit, 2/native-unsigned-integer-unit:8).
-define(PRIs16bit, 2/native-signed-integer-unit:8).
-define(PRIu32bit, 4/native-unsigned-integer-unit:8).
-define(PRIs32bit, 4/native-signed-integer-unit:8).
-define(PRIp16bit, 4/native-signed-integer-unit:8).
-define(PRIp32bit, 4/native-signed-integer-unit:8).

%% array sizes
-define(PRI_MAX_LINES, 8).
-define(PRI_NUM_DS1_INTERFACES, 20).
-define(PRI_MAX_SPID_LEN, 20).
-define(PRI_MAX_DN_LEN, 20).
-define(PRI_MAX_BOND_CHAN, 20).

%%
%% L4L3 Common Headers 
%%
-record('L4_to_L3_struct',
		{lapdid=0, msgtype=0, l4_ref=0, call_ref=0, lli=0, data=undefined}).

%%
%% L3L4 Common Headers 
%%
-record('L3_to_L4_struct',
		{lapdid=0, msgtype=0, l4_ref=0, call_ref=0, bchanel=0,
		iface=0, bchannel_mask=0, lli=0, data_channel=0, data=0}).

%%% TODO:  deprecate these macros?
%%
%% defines for the L4L3 & L3L4 Common Headers 
%%
-define(L4L3_Mask(LapdId, MsgType, L4Ref, CallRef, Lli),
		<<0, LapdId:?PRIu8bit, MsgType:?PRIu8bit, L4Ref:?PRIu16bit,
		CallRef:?PRIu16bit, Lli:?PRIu16bit>>).
-define(L3L4_Mask(LapdId, MsgType, L4Ref, CallRef, BChan,
		Iface, BChanMask, Lli, DataChan, Data),
		<<LapdId:?PRIu8bit, MsgType:?PRIu8bit, L4Ref:?PRIu16bit,
		CallRef:?PRIu16bit, BChan:?PRIu8bit, Iface:?PRIu8bit,
		BChanMask:?PRIu32bit, Lli:?PRIu16bit, DataChan:?PRIu16bit,
		Data/binary>>).

%%% TODO:  how to organize these macros/records/functions
-define(L3mENABLE_PROTOCOL,  16#B6).

-define(L4L3mENABLE_PROTOCOL,  16#B6).
-record('PRI_ENA_PROTO_DATA',
	{command=0, command_parameter=0, level1, level2, level3}).


%%
%% error in response to L4L3mXXXXX
%%
-define(L3L4mERROR, 16#20). 
-define(L3L4mErrorMsg(X), element(2, element(2, lists:keysearch(X, 1,
		[{0, no_erorr}, {1, lapdid_out_of_range},
		{2, lapdid_not_established}, {3, invalid_called_number},
		{4, no_crv_available}, {5, no_crstruct_available},
		{6, call_ref_error}, {7, invalid_b_channel},
		{8, b_chanel_restarting}, {9, b_chanel_oos}, 
		{10, invalid_call_type}, {11, invalid_conn_type},
		{12, protocol_not_disabled}, {13, invalid_hdlc_maping},
		{14, invalid_data_queue}, {15, invalid_comand_args},
		{16, invalid_msg_for_state}, {17, data_packet_lost},
		{18, pm_not_esf}, {19, invalid_interface},
		{20, b_channel_inuse}, {21, invalid_lli},
		{22, vc_table_full}, {23, lli_not_found}, {24, blocked},
		{25, no_hardware}, {26, invalid_spid_len}, {27, non_nfas},
		{28, invalid_state}, {29, service_not_offered},
		{30, dchan_temp_unavail}, {31, too_many_q931_stacks},
		{32, service_not_config}, {33, data_interface_required},
		{34, data_interface_invalid}, {35, sym_mode_not_supported},
		{36, invalid_bufsz}, {37, bond_chan_not_cnfg},
		{38, bond_chan_bit_conflict}, {39, bond_wrong_spyder_chip},
		{40, bond_too_many_channels}, {41, bond_dup_addon_chan},
		{42, dchan_odd_pointer_error}, {43, dchan_too_few_buffers},
		{44, dchan_too_many_buffers}, {45, dchan_give_take_nonzero},
		{46, dchan_zero_rxbuf_len}, {47, sym_mode_required},
		{48, dlci_manditory}, {49, chan_kbit_rate_bad},
		{50, invalid_mem_size}, {51, not_enough_memory},
		{52, tx_buffer_misaligned}, {53, x_buffer_misaligned},
		{54, too_many_dlcis}, {55, bond_bad_state}, {56, spid_rejected},
		{57, tei_ident_remove_req}, {58, spid2_rejected},
		{59, invalid_smi_msgid}, {60, invalid_clock_mode},
		{61, no_overflow_queue}, {62, too_many_cas_dest},
		{63, segment_too_large}, {64, segment_message_expected},
		{65, segment_message_invalid}, {66, segment_timer_expired},
		{67, invalid_download_msg}, {68, protocol_disabled},
		{69, invalid_variant}, {70, too_many_links}, {71, too_many_headers},
		{72, fatal_error}, {73, hot_swap_extraction},
		{74, dchan_out_of_range}])))).


%%
%% defines for set_hardware/2 and req_hw_status/1
%%
-record('PRI_LINE_DATA',
		{framing=0, line_code=0, pm_mode=0, line_length=0, term=0,
		line_type=0, integrate_alarms=0, filter_unsolicited=0,
		filter_yellow=0, bri_l1mode=0, briL1_cmd=0, bri_loop=0, 
		bril1_t3=0, bril1_t4=0}).
-record('PRI_HARDWARE_DATA',
		{clocking=0, clocking2=0, enable_clocking2=0, 
		netref_clocking=0, netref_rate=0, ctbus_mode=0,
		force_framer_init=0, tdm_rate=0,
		enable_8370_rliu_monitor=0, dbcount=0, 
		enable_t810x_snap_mode=0, clk_status=0, line_data, csu}).
-define(L4L3mSET_HARDWARE, 16#A7). 
-define(L4L3mREQ_HW_STATUS, 16#A8). 
-define(L3L4mHARDWARE_STATUS, 16#24). 
-define(PRI_HARDWARE_DATA,
		<<HardwareBin:12/binary,
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


%%
%% defines for set_tsi/2 and req_tsi/1
%%
-define(L4L3mSET_TSI, 16#A9). 
-define(L4L3mREQ_TSI_STATUS, 16#D4). 
-define(L3L4mTSI_STATUS, 16#4A). 
-define(PRI_MAX_SET_TSI, 120).
-define(PRItsiGRANULARITY_64K, 16#00).
-define(PRItsiGRANULARITY_32K, 16#01).
-define(PRItsiGRANULARITY_16K, 16#02).
-define(PRItsiGRANULARITY_8K, 16#03).
-define(PRItsiDSi0, 16#0000).
-define(PRItsiDSi1, 16#0100).
-define(PRItsiDSi2, 16#0200).
-define(PRItsiDSi3, 16#0300).
-define(PRItsiDSi4, 16#0400).
-define(PRItsiDSi5, 16#0500).
-define(PRItsiDSi6, 16#0600).
-define(PRItsiDSi7, 16#0700).
-define(PRItsiDSo0, 16#0800).
-define(PRItsiDSo1, 16#0900).
-define(PRItsiDSo2, 16#0a00).
-define(PRItsiDSo3, 16#0b00).
-define(PRItsiDSo4, 16#0c00).
-define(PRItsiDSo5, 16#0d00).
-define(PRItsiDSo6, 16#0e00).
-define(PRItsiDSo7, 16#0f00).
-define(PRItsiLINE_A, 16#1000).
-define(PRItsiLINE_B, 16#1100).
-define(PRItsiLINE_C, 16#1200).
-define(PRItsiLINE_D, 16#1300).
-define(PRItsiLINE_E, 16#1400).
-define(PRItsiLINE_F, 16#1500).
-define(PRItsiLINE_G, 16#1600).
-define(PRItsiLINE_H, 16#1700).
-define(PRItsiHDLC_0, 16#2000).
-define(PRItsiHDLC_1, 16#2100).
-define(PRItsiHDLC_2, 16#2200).
-define(PRItsiHDLC_3, 16#2300).
-define(PRItsiHDLC_4, 16#2400).
-define(PRItsiHDLC_5, 16#2500).
-define(PRItsiHDLC_6, 16#2600).
-define(PRItsiHDLC_7, 16#2700).
-define(PRItsiHDLC_8, 16#2800).
-define(PRItsiHDLC_9, 16#2900).
-define(PRItsiHDLC_10, 16#2a00).
-define(PRItsiHDLC_11, 16#2b00).
-define(PRItsiHDLC_12, 16#2c00).
-define(PRItsiHDLC_13, 16#2d00).
-define(PRItsiHDLC_14, 16#2e00).
-define(PRItsiHDLC_15, 16#2f00).
-define(PRItsiHDLC_16, 16#3000).
-define(PRItsiMODEM_0, 16#6000).
-define(PRItsiMODEM_1, 16#6100).
-define(PRItsiDISABLE, 16#4000).
-define(PRItsiPATTERN, 16#5000).
-define(PRItsiDSP_0, 16#7000).
-define(PRItsiDSP_1, 16#7100).
-define(PRItsiDSP_2, 16#7200).
-define(PRItsiDSP_3, 16#7300).
-define(PRItsiDSP_4, 16#7400).
-define(PRItsiDSP_5, 16#7500).
-define(PRItsiDSP_6, 16#7600).
-define(PRItsiDSP_7, 16#7700).
-define(PRItsiDSi(X), (X bsl 8)).
-define(PRItsiDSo(X), ((X + 8) bsl 8)).
-define(PRItsiLINE(X), 	(16#1000 bor (X bsl 8)).
-define(PRItsiHDLC(X), 	(16#2000 bor (X bsl 8)).
-define(PRItsiMODEM(X), (16#6000 bor (X bsl 8)).
-define(PRItsiDSP(X), 	(16#7000 bor (X bsl 8)).
-define(PRItsiCTd(X), 	(16#8000 bor (X bsl 10)).
-define(PRI_TSI_DATA,
		<<NumMappings:?PRIu16bit, Granularity:?PRIu8bit, Last:?PRIu8bit,
		TsiMapBins/binary>>).
-define(TsiMapMask, <<Destination:?PRIu16bit, Source:?PRIu16bit>>).
-define(TsiMapTerms, [{destination, Destination}, {source, Source}]).



%%
%% PRI_LEVEL1_CNFG
%%
-record('PRI_LEVEL1_CNFG',
      {l1_mode=0, invert_hdlc=0, num_txbuf=0, num_rxbuf=0,
      buffsz=0, chain=0, device=0, bit_reverse=0, vme_lock=0, 
      hdlc_channels=0, chan_kbit_rate=0, crc_bytes=0, 
      crc_ignore_errs=0,
		rate_adapt = #rate_adapt{},
		raw_fillchar=#raw_fillchar{},
		hdlc_flag_fill=#hdlc_flag_fill{},
		modem=#modem{},
		v110=#v110{}}).
-record(rate_adapt, {enable=0, rate_adapt_value=0}).
-record(raw_fillchar, {enable=0, fill_value=0}).
-record(hdlc_flag_fill, {enable=0, mode=0, value=0}).
-record(modem, {originate=0, faxClass=0, encoding=0, amf=0,
		amf_params=#amf_params{}, minBPS=0, maxBPS=0}).
-record(amf_params, {'0'=0, '1'=0, '2'=0, '3'=0}).
-record(v110, {bit_rate=0, auto_detect=0}).


%%
%% PRI_LEVEL2_CNFG
%%
-define(PRIl2modLAP_D,       0).
-define(PRIl2modDISABLED,    1).
-define(PRIl2modLAP_B,       2).
-define(PRIl2modBONDING,     3).
-define(PRIl2modLAP_F,       4).
-define(PRIl2modDPNSS,       5).
-define(PRIl2modLAP_D_EFA,   6).
-define(PRIl2modPM,          7).
-define(PRIl2modSS7,         8).
-define(PRIl2modUDP_IP,      9).
-define(PRIl2modDASS,       10).
-define(PRIl2modV110,       11).
-define(PRIl2modV120,       12).
-define(PRIl2modLAP_F_CORE, 13).
-define(PRIl2modSS7_MON,    14).
-define(PRIdirUSER_SIDE,     0).
-define(PRIdirNETWORK_SIDE,  1).
-define(PRIdirSYMMETRIC,     2).

-record('PRI_LEVEL2_CNFG',
		{par=#'PRI_L2_LAP_PARAMS'{},
		data_interface=#'PRI_DATA_INTERFACE'{},
		consts=#'PRI_L2_LAP_CONSTS'{}}).
-record('PRI_L2_LAP_PARAMS',
		{mode=?PRIl2modLAP_D, dce_dte=0, tei_mode=0, no_sabme=0,
		l2_detail=0, timestamp=0, ui_mode=0, priority=0, no_reestab=0,
		mode_1tr6=0, mode_tei_1=0, no_piggyback=0}).
-record('PRI_L2_LAP_CONSTS',
		{t200=0, t201=0, t202=0, t203=0, n200=0, n201=0, n202=0, k=0}).
-record('PRI_L2_SS7_PARAMS',
		{mode=?PRIl2modSS7, variant=0}).
-record('PRI_L2_SS7_CONSTS', 
		{t1=0, t2=0, t3=0, t4n=0, t4e=0, t5=0, t6=0, t7=0}).
-record('PRI_L2_UDPIP_PARAMS',
		{mode=?PRIl2modUDP_IP, dstport=0, dstipaddr=0}).
-record('PRI_L2_IP_CONSTS',
		{no_dhcp=0, ipaddr=0, gwaddr=0, subnet_mask=0}).
-record('PRI_L2_DPNSS_PARAMS',
		{mode=?PRIl2modDPNSS, pbx_b=0, sabmr_as_ack=0, tie_line_mode=0}).
-record('PRI_L2_DPNSS_CONSTS',
		{nl=0, nt1=0, nt2=0}).
-record('PRI_L2_V110_PARAMS',
		{mode=?PRIl2modV110, ebits=0, flow_control=0, 
		nine_byte_rx_frames=0, num_tx_idle_frames=0, max_rx_frame_size=0,
		stale_rx_data_timer=0, filter_status_messages=0}).
-record('PRI_DATA_INTERFACE',
		{enable=0, data_channel=0, fillandspill=0, allow_buffer_preload=0}).

%%
%% PRI_LEVEL3_CNFG
%%
-define(PRIl3modDISABLED,     0).
-define(PRIl3modQ931,         1).
-define(PRIl3modX25_PKT,      2).
-define(PRIl3modQ933,         3).
-define(PRIl3modBONDING,      4).
-define(PRIl3modPM,           5).
-define(PRIl3modRELAY,        6).
-define(PRIl3modDPNSS,        7).
-define(PRIl3modDASS,         8).
-define(PRIl3modQ933_ANNEX_A, 9).

% switch_type
-define(PRIstATT_4ESS,        16#00).
-define(PRIstATT_5ESS,        16#01).
-define(PRIstNTI_DMS100,      16#02).
-define(PRIstNTI_DMS250,      16#03).
-define(PRIstMD110_T1,        16#04).
-define(PRIstMD110_E1,        16#05).
-define(PRIstSIEMENS,         16#06).
-define(PRIstNTT,             16#07).
-define(PRIstUNKNOWN,         16#08).
-define(PRIstMAX_SWITCH_TYP,  16#08).

% variant
-define(PRIvarATT_CUSTOM,     16#00).
-define(PRIvarNTI_CUSTOM,     16#01).
-define(PRIvarNATL_ISDN_1,    16#02).
-define(PRIvarNATL_ISDN_2,    16#03).
-define(PRIvarJATE,           16#04).
-define(PRIvarCTR3,           16#05).
-define(PRIvarNET3,           16#05).
-define(PRIvarCTR4,           16#06).
-define(PRIvarNET5,           16#06).
-define(PRIvar1TR6_BRI,       16#07).
-define(PRIvar1TR6_PRI,       16#08).
-define(PRIvarVN3,            16#09).
-define(PRIvarITU,            16#0A).
-define(PRIvarCCITT,          16#0A).
-define(PRIvarQ933,           16#0B).
-define(PRIvarQ933_T123,      16#0C).
-define(PRIvarTS014,          16#0D).
-define(PRIvarTS013,          16#0E).
-define(PRIvarARINC_746,      16#0F).
-define(PRIvarGR_303,         16#10).
-define(PRIvarGR_303_TMC,     16#11).

-record('PRI_LEVEL3_CNFG',
		{l3_mode=0, cnfg=#'PRI_Q931_CNFG'{}}).
-record('PRI_Q931_CNFG',
		{switch_type=0, variant=0, call_filtering=0,
		q931_timers=#'PRI_Q931_TIMERS'{},
		b_channel_service_state=lists:duplicate(?PRI_NUM_DS1_INTERFACES, 0),
		nfas=0, e1_30_bchan=0, basic_rate=0, net_side_emul=0, 
		b_chan_negot=0, proc_on_exclusv=0, chanid_slot_map=0,
		sprs_chanid_callproc=0, no_chanid_callproc=0, append_raw_qmsg=0,
		ccitt_mode=0, raw_qmsg=0, no_ie_errcheck=0, user_ie_encode=0,
		overlap_rcv=0, send_l3l4_callproc=0, sending_cmplt=0, 
		require_send_complete=0, report_incoming_callproc=0,
		no_tx_conn_ack=0, no_rx_conn_ack=0, sprs_chanid_setupack=0,
		no_chanid_setupack=0, no_canned_spid_rej=0, call_reject_notify=0,
		advice_of_charge=0, message_segmentation=0, no_bc_user_info=0,
		incoming_call_slot_map=0, release_complete_control=0,
		primary_lapdid=0, backup_lapdid=0, primary_ifnum=0,
		backup_ifnum=0, backup_control=0, spid_len=0, spid_1_len=0,
		dn_len=0, dn_1_len=0, 
		spid=lists:duplicate(?PRI_MAX_SPID_LEN, 0),
		spid_1=lists:duplicate(?PRI_MAX_SPID_LEN, 0),
		dn=lists:duplicate(?PRI_MAX_DN_LEN, 0),
		dn_1=lists:duplicate(?PRI_MAX_DN_LEN, 0),
		chan_id_high_bit=0, att_cust_bri_ekts=0, subscribe_connack=0,
		suppress_auto_spid=0, accept_all_bri_calls=0}).
-record('PRI_Q931_TIMERS',
		{t302=0, t305=0, t308=0, t313=0, t314=0, t316=0, t318=0, t319=0,
		t3m1=0, t321=0}).
-record('PRI_BONDING_DATA',
		{mode=0, destination=0, num_tx_buf=0, num_rx_buf=0,
		data_channel=0, txinit=0, txadd01=0, txfa=0, txdisc=0,
		txdeq=0, tcid=0, tanull=0, channels=0, 
		directory=lists:duplicate(?PRI_MAX_BOND_CHAN, 0)}).
-record('PRI_X25_CONFIG',
		{cfg_msk=0, t10=0, t11=0, t12=0, t13=0, t28=0, p=0, w=0,
		max_clr_retry=0, max_svcs=0, max_pvcs=0}).
-record('PRI_PM_CONFIG',
		{mode=0, carrier=0, fdl_alert=0, 
		equipmentid=lists:duplicate(10, 0),
		locationid=lists:duplicate(11, 0),
		frameid=lists:duplicate(10, 0),
		unitid=lists:duplicate(6, 0),
		facilityid=lists:duplicate(38, 0)}).
-record('PRI_RELAY_CONFIG',
		{default_dest=0, default_dest_id=0, default_root_idx=0}).
-record('PRI_DPNSSCC_CONFIG',
		{pbx_y=0, no_virtual_channels=0, dest_addr_len=0, 
		b_channel_service_state=0, v_channel_service_state=0,
		t_i_msg=0, t_guard=0}).
-record('PRI_DASSCC_CONFIG',
		{b_channel_service_state=0, t_digit_racking=0, n_clear_retries=0}).
-record('PRI_Q933A_CONFIG',
		{network_side=0, n391=0, n392=0, n393=0, t391=0, t392=0}).


-record('PRI_DATA_INTERFACE_CONFIGURATION',
		{dchan_descr_addr=0, num_dchan_descr=0,
				dchan_event_queue_addr=0, num_l3l4_dchan_events=0,
				num_l4l3_dchan_events=0}).
