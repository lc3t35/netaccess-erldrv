%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% naii.hrl   Erlang header file defining the structures in naii.h     %%%
%%%                                                                     %%%
%%%---------------------------------------------------------------------%%%
%%% Copyright Motivity Telecom Inc. 2001, 2002, 2003
%%%
%%% All rights reserved. No part of this computer program(s) may be
%%% used, reproduced, stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of
%%% Motivity Telecom Inc.
%%%---------------------------------------------------------------------


%% ioctl commands
-define(BOOT_BOARD, 0).
-define(ENABLE_MANAGEMENT_CHAN, 1).
-define(RESET_BOARD,  2).
-define(GET_VERSION,  3).
-define(GET_DRIVER_INFO, 4).
-define(SELECT_BOARD, 5).
-define(CANCEL_ASYNC, 10).

-define(PRIs8bit, 8/native-signed-integer).
-define(PRIu8bit, 8/native-unsigned-integer).
-define(PRIs16bit, 16/native-signed-integer).
-define(PRIu16bit, 16/native-unsigned-integer).
-define(PRIs32bit, 32/native-signed-integer).
-define(PRIu32bit, 32/native-unsigned-integer).

-define(PRI_MAX_LINES, 8).

%%
%% defines for get_driver_info/2
%%
-define(DriverInfoMask,
		<<BoardType:?SIZEINT/native-signed-integer-unit:8,
		HangUpOnRedAlarm:?SIZEINT/native-signed-integer-unit:8,
		FlowControlBoard:?SIZEINT/native-signed-integer-unit:8,
		FlowControlWsrv:?SIZEINT/native-signed-integer-unit:8,
		FlowControlRsrv:?SIZEINT/native-signed-integer-unit:8,
		HDrops:?SIZEINT/native-signed-integer-unit:8,
		SDrops:?SIZEINT/native-signed-integer-unit:8,
		TxMsgSize:?SIZEINT/native-signed-integer-unit:8,
		RxMsgSize:?SIZEINT/native-signed-integer-unit:8,
		TxNumBufs:?SIZEUSHORT/native-unsigned-integer-unit:8,
		RxNumBufs:?SIZEUSHORT/native-unsigned-integer-unit:8,
		MaxDataChannels:?SIZEUINT/native-unsigned-integer-unit:8>>).
-define(DriverInfoTerms,
		[{board_type, BoardType}, {hangup_on_red_alarm, HangUpOnRedAlarm},
		{flow_control_board, FlowControlBoard},
		{flow_control_wsrv, FlowControlWsrv},
		{flow_control_rsrv, FlowControlRsrv},
		{hdrops, HDrops}, {sdrops, SDrops},
		{tx_msg_size, TxMsgSize}, {rx_msg_size, RxMsgSize},
		{tx_num_bufs, TxNumBufs},	{rx_num_bufs, RxNumBufs},
		{max_data_channels, MaxDataChannels}]).

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
%% defines for enable_protocol/
%%
-define(L4L3mENABLE_PROTOCOL, 16#B6).
-define(PRI_ENA_PROTO_DATA, 
		<<Command:?PRIu16bit, CommandParameter:?PRIu16bit,
		  Level1:?PRI_LEVEL1_CNFG, Level2:?PRI_LEVEL2_CNFG,
		  Level3:?PRI_LEVEL3_CNFG>>).


