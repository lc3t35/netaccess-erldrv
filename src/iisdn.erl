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
%%% @doc Conversion routines for the API of the netaccess application.
%%%
%%%	<p>This module provides functions which convert the records used
%%% 	in the API to the binary format of data exchanged with the boards.
%%% 	The erlang driver passes L4L3m messages to the board and receives
%%% 	L3L4m messages from the board.  These binary messages are specified
%%% 	in a C language API using structures, arrays, longs etc.</p>
%%%
%%% 	<p>This module uses a record for each of the C structures documented
%%% 	in the Instant ISDN&#153; SMI Reference Guide and appearing in the 
%%%   <tt>iisdn.h</tt> header file provided with the board's drivers. 
%%% 	The records are defined in <tt>iisdn.hrl</tt>.  Each record has a
%%% 	corresponding function in this module of the same name which
%%% 	takes the record and returns a properly packed binary which is 
%%% 	equivalent to what the C API would have created.  This binary is 
%%% 	passed to the board as received by the driver.  This is quite
%%% 	effecient as the memory is allocated once in the emulator and 
%%% 	reference passed to the driver which sends it down the stream to 
%%% 	the board.  Portability between architectures is handled in the
%%% 	driver's build system using GNU autotools (autoconf, automake,
%%% 	etc.).  The iisdn.hrl header file is generated automatically during
%%% 	the build process.</p>
%%%
%%% 	<p>Some functions encode a record into a binary, others decode 
%%% 	a binary into a record.  Others work both ways, if you pass it
%%% 	a record it will return a binary but if you pass it a binary it
%%% 	will return a record.  The choice is determined by the context
%%% 	in which the function is used.</p>
%%%
%%% @reference Instant ISDN&#153; SMI Programmer's Guide
%%% @reference Instant ISDN&#153; SMI Reference Guide
%%%

-module(iisdn).

-export([l3_to_l4/1, l4_to_l3/1]).
-export([error_code/1]).
-export([ena_proto_data/1]).
-export([hardware_data/1, line_data/1]).
-export([tsi_data/1, tsi_map/1]).
-export([q931_timers/1]).
-export([level1/1, level2/1, level3/1]).
-export([l2_lap_params/1, l2_ss7_params/1, l2_udpip_params/1,
		l2_tcpip_params/1, l2_dpnss_params/1, l2_v110_params/1]).
-export([data_interface/1]).
-export([q931/1, bonding_data/1, x25/1, pm/1,
		relay/1, dpnss/1, dass/1,
		q933a/1]).
-export([l2_lap_consts/1, l2_ss7_consts/1, l2_ip_consts/1,
	 	l2_dpnss_consts/1]).
-export([protocol_stat/1, q933a_pvc_status/1]).
-export([board_id/1]).
-export([l2_stats/1, mtp2_stats/1, l2_mtp2_stats/1]).
-export([alarm_status/1, line_status/1]).

-include("iisdn.hrl").


%% @type l4_to_l3().  L4L3 SMI Message sent from host to board.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>lapdid</tt></dt><dd><tt>integer()</tt>
%% 				Identifies the physical HDLC channel to be used.</dd>
%% 		<dt><tt>msgtype</tt></dt><dd><tt>integer()</tt>
%% 				Identifies the L4L3m message being sent.</dd>
%% 		<dt><tt>l4_ref</tt></dt><dd><tt>integer()</tt>
%% 				Reference assigned to outgoing calls by the host application.
%% 				Subsequent L3L4m for the call will use this value. Used in
%% 				call control applications only.</dd>
%% 		<dt><tt>call_ref</tt></dt><dd><tt>integer()</tt>
%% 				Call reference assigned by the board.  Used in
%% 				call control applications only.</dd>
%% 		<dt><tt>lli</tt></dt><dd><tt>integer()</tt>
%% 				Logical Link ID or DLCI.  Used wth LAPD, LAPB and V.120.</dd>
%% 	</dl>
%%
%% @spec (L4L3mRec) -> L4L3mBin
%% 	L4L3mRec = l4_to_l3()
%% 	L4L3mBin = binary()
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
	<<Header/binary, Data/binary>>;
l4_to_l3(?L4L3mSET_HARDWARE, Header, Data) ->
	<<Header/binary, (hardware_data(Data))/binary>>;
l4_to_l3(?L4L3mSET_TSI, Header, Data) ->
	<<Header/binary, (tsi_data(Data))/binary>>;
l4_to_l3(?L4L3mENABLE_PROTOCOL, Header, Data) ->
	<<Header/binary, (ena_proto_data(Data))/binary>>.

%% @type l3_to_l4().  L3L4 SMI Message sent from board to host.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>lapdid</tt></dt><dd><tt>integer()</tt>
%% 				Identifies the physical HDLC channel to be used.</dd>
%% 		<dt><tt>msgtype</tt></dt><dd><tt>integer()</tt>
%% 				Identifies the L3L4m message being received.</dd>
%% 		<dt><tt>l4_ref</tt></dt><dd><tt>integer()</tt>
%% 				Reference previously assigned to outgoing calls by the host application.
%% 				Used in call control applications only. 16#ffff if unused.</dd>
%% 		<dt><tt>call_ref</tt></dt><dd><tt>integer()</tt>
%% 				Call reference assigned by the board.  Used in
%% 				call control applications only.</dd>
%% 		<dt><tt>bchanel</tt></dt><dd><tt>integer()</tt>
%% 				Identifies the bearer channel.</dd>
%% 		<dt><tt>interface</tt></dt><dd><tt>integer()</tt>
%% 				Non-Facility Associated Signaling (NFAS) interface (0-19).
%% 				Used in Q.931 NFAS applications only.  16#ff if unused.</dd>
%% 		<dt><tt>bchannel_mask</tt></dt><dd><tt>integer()</tt>
%% 				B-channel bit mask.  Used in mutirate ISDN calls only.</dd>
%% 		<dt><tt>lli</tt></dt><dd><tt>integer()</tt>
%% 				Logical Link ID or DLCI.  Used wth LAPD, LAPB and V.120.</dd>
%% 		<dt><tt>data_channel</tt></dt><dd><tt>integer()</tt>
%% 				Identifies the data stream.  Unused in Solaris driver implementation.</dd>
%% 		<dt><tt>data</tt></dt><dd><tt>binary()</tt>
%%				The message specific data being received.</dd>
%% 	</dl>
%%
%% @spec (L3L4mBin) -> L3L4mRec
%% 	L3L4mBin = binary()
%% 	L3L4mRec = l3_to_l4()
%%
l3_to_l4(Bin) when is_binary(Bin) ->
	<<Lapdid:?IISDNu8bit, Msgtype:?IISDNu8bit, L4_ref:?IISDNu16bit,
		Call_ref:?IISDNu16bit, Bchanel:?IISDNu8bit, Iface:?IISDNu8bit,
		Bchannel_mask:?IISDNu32bit, Lli:?IISDNu16bit, Data_channel:?IISDNu16bit,
		Data/binary>> = Bin,
	#l3_to_l4{lapdid=Lapdid, msgtype=Msgtype, l4_ref=L4_ref,
			call_ref=Call_ref, bchanel=Bchanel, interface=Iface,
			bchannel_mask=Bchannel_mask, lli=Lli, 
			data_channel=Data_channel, data=Data}.

%% @type error_code().  L3L4mERROR error code.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>error_code</tt></dt> <dd><tt>no_error | lapdid_out_of_range | lapdid_not_established | invalid_called_number
%% 			| no_crv_available | no_crstruct_available | call_ref_error | invalid_b_channel 
%% 			| b_chanel_restarting | b_chanel_oos | invalid_call_type | invalid_conn_type
%% 			| protocol_not_disabled invalid_hdlc_maping | invalid_data_queue | invalid_comand_args
%% 			| invalid_msg_for_state | data_packet_lost | pm_not_esf | invalid_interface
%% 			| b_channel_inuse | invalid_lli | vc_table_full | lli_not_found | blocked
%% 			| no_hardware | invalid_spid_len | non_nfas | invalid_state | service_not_offered 
%% 			| dchan_temp_unavail | too_many_q931_stacks | service_not_offered 
%% 			| data_interface_required data_interface_invalid | sym_mode_not_supported
%% 			| invalid_bufsz | bond_chan_not_cnfg | bond_chan_bit_conflict
%% 			| bond_wrong_spyder_chip | bond_too_many_channels | bond_dup_addon_chan
%% 			| dchan_odd_pointer_error | dchan_too_few_buffers | dchan_too_many_buffers
%% 			| dchan_give_take_nonzero | dchan_zero_rxbuf_len | sym_mode_required
%% 			| dlci_manditory | chan_kbit_rate_bad | invalid_mem_size | not_enough_memory 
%% 			| tx_buffer_misaligned | x_buffer_misaligned | too_many_dlcis | bond_bad_state
%% 			| spid_rejected | tei_ident_remove_req | spid2_rejected | invalid_smi_msgid 
%% 			| invalid_clock_mode | no_overflow_queue | too_many_cas_dest | segment_too_large 
%% 			| segment_message_expected | segment_message_invalid | segment_timer_expired 
%% 			| invalid_download_msg | protocol_disabled | invalid_variant | too_many_links 
%% 			| too_many_headers | fatal_error | hot_swap_extraction | dchan_out_of_range 
%% 			| ether_already_configured | tsi_verification_failed | status_ignored 
%% 			| bad_call_ref | glare | integer()</tt></dd>
%% 		<dt><tt>offending_message</tt></dt> <dd>Msgtype value of the offending L3L4 message</dd>
%% 	</dl>
%%
%% @spec (ErrorCodeBin) -> ErrorCodeRec
%% 	ErrorCodeBin = binary()
%% 	ErrorCodeRec = error_code()
%%
error_code(B) when is_binary(B) ->
	<<ErrorCode:?IISDNu8bit, OffendingMessage:?IISDNu8bit, _Rest/binary>> = B,
	ErrorCodes = [{0, no_error}, {1, lapdid_out_of_range},
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
			{32, service_not_offered}, {33, data_interface_required},
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
			{74, dchan_out_of_range}, {75, ether_already_configured},
			{76, tsi_verification_failed},
			{100, status_ignored}, {101, bad_call_ref},
			{102, glare}],
	ErrorName = case element(2, element(2, lists:keysearch(ErrorCode, 1, ErrorCodes))) of
		Atom when is_atom(Atom) ->
			Atom;
		false ->
			ErrorCode
	end,
	#error_code{error_code = ErrorName, offending_message = OffendingMessage}.


%% @type board_id(). Board identification.
%% 	<p>A record which includes the following fields:</p>
%%		<dl>
%%			<dt><tt>iisdn_ver</tt></dt> <dd><tt>string()</tt></dd>
%%			<dt><tt>banner</tt></dt> <dd><tt>string()</tt></dd>
%%			<dt><tt>date</tt></dt> <dd><tt>string()</tt></dd>
%%			<dt><tt>model</tt></dt> <dd><tt>string()</tt></dd>
%%			<dt><tt>rev</tt></dt> <dd><tt>string()</tt></dd>
%%			<dt><tt>board_type</tt></dt> <dd><tt>integer()</tt></dd>
%%			<dt><tt>num_lines</tt></dt> <dd><tt>integer()</tt></dd>
%%			<dt><tt>num_hdlc_chan</tt></dt> <dd><tt>integer()</tt></dd>
%%			<dt><tt>num_modem_chan</tt></dt> <dd><tt>integer()</tt></dd>
%%			<dt><tt>line_type</tt></dt> <dd><tt>[line_type()]</tt></dd>
%%			<dt><tt>kernel_ram_size</tt></dt> <dd><tt>integer()</tt></dd>
%%			<dt><tt>mezz_ram_size</tt></dt> <dd><tt>integer()</tt></dd>
%%			<dt><tt>num_bfio_devices</tt></dt> <dd><tt>integer()</tt></dd>
%%		</dl>
%%
%% @spec(BoardIdBin) -> BoardIdRec
%% 	BoardIdBin = binary()
%% 	BoardIdRec = board_id()
%%
board_id(B) when is_binary(B) ->
	Size_32 = (32 * ?SIZEOF_IISDNu8bit),
	Size_16 = (16 * ?SIZEOF_IISDNu8bit),
	Size_12 = (12 * ?SIZEOF_IISDNu8bit),
	Size_lines = (?IISDN_MAX_LINES * ?SIZEOF_IISDNu8bit),
	<<IISDNVer:Size_32/binary, Banner:Size_32/binary, Date:Size_16/binary,
			Model:Size_16/binary, Rev:Size_12/binary, Board_type:?IISDNu8bit,
			Num_lines:?IISDNu8bit, _:?IISDNu8bit, _:?IISDNu8bit, 
			Num_hdlc_chan:?IISDNu16bit, Num_modem_chan:?IISDNu16bit,
			Line_type:Size_lines/binary, Kernel_ram_size:?IISDNu32bit,
			Mezz_ram_size:?IISDNu32bit, Num_bfio_devices:?IISDNu8bit,
			_:?IISDNu8bit, _:?IISDNu8bit, _:?IISDNu8bit, _Rest/binary>> = B,
	#board_id{iisdn_ver = lists:takewhile(fun(I) -> I /= 0 end, binary_to_list(IISDNVer)),
			banner  = lists:takewhile(fun(I) -> I /= 0 end, binary_to_list(Banner)),
			date = lists:takewhile(fun(I) -> I /= 0 end, binary_to_list(Date)),
			model = lists:takewhile(fun(I) -> I /= 0 end, binary_to_list(Model)),
			rev = lists:takewhile(fun(I) -> I /= 0 end, binary_to_list(Rev)),
			board_type = Board_type, num_lines = Num_lines,
			num_hdlc_chan = Num_hdlc_chan, num_modem_chan = Num_modem_chan,
			line_type = lists:takewhile(fun(I) -> I /= 0 end, line_type(Line_type)),
			kernel_ram_size = Kernel_ram_size, mezz_ram_size = Mezz_ram_size,
			num_bfio_devices = Num_bfio_devices}.
			

%% @type level1().  Layer 1 configuration.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>l1_mode</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>invert_hdlc</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>num_txbuf</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>num_rxbuf</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>buffsz</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>chain</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>device</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>bit_reverse</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>vme_lock</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>hdlc_channels</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>chan_kbit_rate</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>crc_bytes</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>crc_ignore_errs</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>crc_ignore_errs</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>rate_adapt</tt></dt><dd><tt>rate_adapt()</tt></dd>
%% 		<dt><tt>raw_fillchar</tt></dt><dd><tt>raw_fillchar()</tt></dd>
%% 		<dt><tt>hdlc_flag_fill</tt></dt><dd><tt>hdlc_flag_fill()</tt></dd>
%% 		<dt><tt>modem</tt></dt><dd><tt>modem()</tt></dd>
%% 		<dt><tt>v110</tt></dt><dd><tt>v110()</tt></dd>
%% 	</dl>
%%
%% @spec (Level1Rec) -> Level1Bin
%% 	Level1Rec = level1()
%% 	Level1Bin = binary()
%%
level1(L1) when is_record(L1, level1) ->
	Digit32 = fun(Digit32, Bin) -> <<Bin/binary, Digit32:?IISDNu32bit>> end,
	AmfParms = lists:foldl(Digit32, <<>>, (L1#level1.modem)#modem.amf_params),
	<<(L1#level1.l1_mode):?IISDNu8bit,
			(L1#level1.invert_hdlc):?IISDNu8bit,
	      (L1#level1.num_txbuf):?IISDNu16bit,
	      (L1#level1.num_rxbuf):?IISDNu16bit,
	      (L1#level1.buffsz):?IISDNu16bit,
	      (L1#level1.chain):?IISDNu8bit,
	      (L1#level1.device):?IISDNu8bit,
	      (L1#level1.bit_reverse):?IISDNu8bit,
	      (L1#level1.vme_lock):?IISDNu8bit,
	      (L1#level1.hdlc_channels):?IISDNu32bit,
	      (L1#level1.chan_kbit_rate):?IISDNu16bit,
	      (L1#level1.crc_bytes):?IISDNu8bit,
	      (L1#level1.crc_ignore_errs):?IISDNu8bit,
			((L1#level1.rate_adapt)#rate_adapt.enable):?IISDNu8bit,
			((L1#level1.rate_adapt)#rate_adapt.rate_adapt_value):?IISDNu8bit,
			0:?IISDNu16bit,
			((L1#level1.raw_fillchar)#raw_fillchar.enable):?IISDNu8bit,
			((L1#level1.raw_fillchar)#raw_fillchar.fill_value):?IISDNu8bit,
			0:?IISDNu16bit,
			((L1#level1.hdlc_flag_fill)#hdlc_flag_fill.enable):?IISDNu8bit,
			((L1#level1.hdlc_flag_fill)#hdlc_flag_fill.mode):?IISDNu8bit,
			((L1#level1.hdlc_flag_fill)#hdlc_flag_fill.value):?IISDNu16bit,
			((L1#level1.modem)#modem.originate):?IISDNu8bit,
			((L1#level1.modem)#modem.faxClass):?IISDNu8bit,
			((L1#level1.modem)#modem.encoding):?IISDNu8bit,
			((L1#level1.modem)#modem.amf):?IISDNu8bit,
			AmfParms/binary,
			((L1#level1.modem)#modem.minBPS):?IISDNu32bit,
			((L1#level1.modem)#modem.maxBPS):?IISDNu32bit,
			((L1#level1.v110)#v110.bit_rate):?IISDNu32bit,
			((L1#level1.v110)#v110.auto_detect):?IISDNu8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit>>.
			

%% @type level2().  Layer 2 configuration.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>par</tt></dt><dd><tt>l2_lap_params() | l2_udpip_params()
%% 				| l2_tcpip_params() | l2_dpnss_params() | l2_ss7_params
%% 				| l2_v110_params()</tt></dd>
%% 		<dt><tt>data_interface</tt></dt><dd><tt>data_interface()</tt></dd>
%% 		<dt><tt>consts</tt></dt><dd><tt>l2_lap_consts() | l2_ip_consts()
%% 				| l2_dpnss_consts() | l2_ss7_consts()</tt></dd>
%% 	</dl>
%%
%% @spec (Level2Rec) -> Level1Bin
%% 	Level2Rec = level2()
%% 	Level2Bin = binary()
%%
level2(L2) when is_record(L2, level2),
		is_record(L2#level2.par, l2_lap_params) ->
	level2(L2#level2{
			par = l2_lap_params(L2#level2.par)});
level2(L2) when is_record(L2, level2),
		is_record(L2#level2.par, l2_udpip_params) ->
	level2(L2#level2{
			par = l2_udpip_params(L2#level2.par)});
level2(L2) when is_record(L2, level2),
		is_record(L2#level2.par, l2_tcpip_params) ->
	level2(L2#level2{
			par = l2_tcpip_params(L2#level2.par)});
level2(L2) when is_record(L2, level2),
		is_record(L2#level2.par, l2_dpnss_params) ->
	level2(L2#level2{
			par = l2_dpnss_params(L2#level2.par)});
level2(L2) when is_record(L2, level2),
		is_record(L2#level2.par, l2_ss7_params) ->
	level2(L2#level2{
			par = l2_ss7_params(L2#level2.par)});
level2(L2) when is_record(L2, level2),
		is_record(L2#level2.par, l2_v110_params) ->
	level2(L2#level2{
			par = l2_v110_params(L2#level2.par)});
level2(L2) when is_record(L2, level2),
		is_record(L2#level2.data_interface, data_interface) ->
	level2(L2#level2{
			data_interface = 
					data_interface(L2#level2.data_interface)});
level2(L2) when is_record(L2, level2),
		is_record(L2#level2.consts, l2_lap_consts) ->
	level2(L2#level2{
			consts = l2_lap_consts(L2#level2.consts)});
level2(L2) when is_record(L2, level2),
		is_record(L2#level2.consts, l2_ip_consts) ->
	level2(L2#level2{
			consts = l2_ip_consts(L2#level2.consts)});
level2(L2) when is_record(L2, level2),
		is_record(L2#level2.consts, l2_dpnss_consts) ->
	level2(L2#level2{
			consts = l2_dpnss_consts(L2#level2.consts)});
level2(L2) when is_record(L2, level2),
		is_record(L2#level2.consts, l2_ss7_consts) ->
	level2(L2#level2{
			consts = l2_ss7_consts(L2#level2.consts)});
level2(L2) when is_record(L2, level2),
		is_binary(L2#level2.par);
		is_binary(L2#level2.data_interface);
		is_binary(L2#level2.consts) ->
	<<(L2#level2.par)/binary,
			(L2#level2.data_interface)/binary,
			(L2#level2.consts)/binary>>.

%% @type data_interface().  Data interface configuration.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>enable</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>fillandspill</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>allow_buffer_preload</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec (DataInterfaceRec) -> DataInterfaceBin
%% 	DataInterfaceRec = data_interface()
%% 	DataInterfaceBin = binary()
%%
data_interface(DataIf) when is_record(DataIf, data_interface) ->
	<<(DataIf#data_interface.enable):?IISDNu8bit,
			(DataIf#data_interface.data_channel):?IISDNu8bit,
			(DataIf#data_interface.fillandspill):?IISDNu8bit,
			(DataIf#data_interface.allow_buffer_preload):?IISDNu8bit>>.

%% @type l2_lap_params().  Layer 2 LAP parameters.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>mode</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>dce_dte</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>tei_mode</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>no_sabme</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>l2_detail</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>timestamp</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>ui_mode</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>priority</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>no_reestab</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>mode_1tr6</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>mode_tei_1</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>no_piggyback</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec (L2LapParamsRec) -> L2LapParamsBin
%% 	L2LapParamsRec = l2_lap_params()
%% 	L2LapParamsBin = binary()
%%
l2_lap_params(Lap) when is_record(Lap, l2_lap_params) ->
	Pad = lists:max([?SIZEOF_IISDN_L2_LAP_PARAMS,
			?SIZEOF_IISDN_L2_UDPIP_PARAMS,
			?SIZEOF_IISDN_L2_TCPIP_PARAMS,
			?SIZEOF_IISDN_L2_DPNSS_PARAMS,
			?SIZEOF_IISDN_L2_SS7_PARAMS,
			?SIZEOF_IISDN_L2_V110_PARAMS]) - ?SIZEOF_IISDN_L2_LAP_PARAMS,
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
			(Lap#l2_lap_params.no_piggyback):?IISDNu8bit,
			0:Pad/integer-unit:8>>.

%% @type l2_udpip_params().  Layer 2 UDP/IP parameters.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>mode</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>dstport</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>dstipaddr</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec (L2UdpIpParamsRec) -> L2UdpIpParamsBin
%% 	L2UdpIpParamsRec = l2_udpip_params()
%% 	L2UdpIpParamsBin = binary()
%%
l2_udpip_params(Udpip) when is_record(Udpip, l2_udpip_params) ->
	Pad = lists:max([?SIZEOF_IISDN_L2_LAP_PARAMS,
			?SIZEOF_IISDN_L2_UDPIP_PARAMS,
			?SIZEOF_IISDN_L2_TCPIP_PARAMS,
			?SIZEOF_IISDN_L2_DPNSS_PARAMS,
			?SIZEOF_IISDN_L2_SS7_PARAMS,
			?SIZEOF_IISDN_L2_V110_PARAMS]) - ?SIZEOF_IISDN_L2_UDPIP_PARAMS,
	<<(Udpip#l2_udpip_params.mode):?IISDNu8bit,
			0:?IISDNu8bit,
			(Udpip#l2_udpip_params.dstport):?IISDNu16bit,
			(Udpip#l2_udpip_params.dstipaddr):?IISDNu32bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit,
			0:Pad/integer-unit:8>>.

%% @type l2_tcpip_params().  Layer 2 TCP/IP parameters.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>mode</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>dstport</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>dstipaddr</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec (L2TcpIpParamsRec) -> L2TcpIpParamsBin
%% 	L2TcpIpParamsRec = l2_tcpip_params()
%% 	L2UTcIpParamsBin = binary()
%%
l2_tcpip_params(Tcpip) when is_record(Tcpip, l2_tcpip_params) ->
	Pad = lists:max([?SIZEOF_IISDN_L2_LAP_PARAMS,
			?SIZEOF_IISDN_L2_UDPIP_PARAMS,
			?SIZEOF_IISDN_L2_TCPIP_PARAMS,
			?SIZEOF_IISDN_L2_DPNSS_PARAMS,
			?SIZEOF_IISDN_L2_SS7_PARAMS,
			?SIZEOF_IISDN_L2_V110_PARAMS]) - ?SIZEOF_IISDN_L2_TCPIP_PARAMS,
	<<(Tcpip#l2_tcpip_params.mode):?IISDNu8bit,
			0:?IISDNu8bit,
			(Tcpip#l2_tcpip_params.dstport):?IISDNu16bit,
			(Tcpip#l2_tcpip_params.dstipaddr):?IISDNu32bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit,
			0:Pad/integer-unit:8>>.

%% @type l2_dpnss_params().  Layer 2 DPNSS parameters.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>mode</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>pbx_b</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>sabmr_as_ack</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>tie_line_mode</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec (L2DpnssParamsRec) -> L2DpnssParamsBin
%% 	L2DpnssParamsRec = l2_dpnss_params()
%% 	L2DpnssParamsBin = binary()
%%
l2_dpnss_params(Dpnss) when is_record(Dpnss, l2_dpnss_params) ->
	Pad = lists:max([?SIZEOF_IISDN_L2_LAP_PARAMS,
			?SIZEOF_IISDN_L2_UDPIP_PARAMS,
			?SIZEOF_IISDN_L2_TCPIP_PARAMS,
			?SIZEOF_IISDN_L2_DPNSS_PARAMS,
			?SIZEOF_IISDN_L2_SS7_PARAMS,
			?SIZEOF_IISDN_L2_V110_PARAMS]) - ?SIZEOF_IISDN_L2_DPNSS_PARAMS,
	<<(Dpnss#l2_dpnss_params.mode):?IISDNu8bit,
			(Dpnss#l2_dpnss_params.pbx_b):?IISDNu8bit,
			(Dpnss#l2_dpnss_params.sabmr_as_ack):?IISDNu8bit,
			(Dpnss#l2_dpnss_params.tie_line_mode):?IISDNu8bit,
			0:Pad/integer-unit:8>>.

%% @type l2_ss7_params().  Layer 2 SS7 parameters.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>mode</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>variant</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec (L2Ss7ParamsRec) -> L2Ss7ParamsBin
%% 	L2Ss7ParamsRec = l2_ss7_params()
%% 	L2Ss7ParamsBin = binary()
%%
l2_ss7_params(Mtp2) when is_record(Mtp2, l2_ss7_params) ->
	Pad = lists:max([?SIZEOF_IISDN_L2_LAP_PARAMS,
			?SIZEOF_IISDN_L2_UDPIP_PARAMS,
			?SIZEOF_IISDN_L2_TCPIP_PARAMS,
			?SIZEOF_IISDN_L2_DPNSS_PARAMS,
			?SIZEOF_IISDN_L2_SS7_PARAMS,
			?SIZEOF_IISDN_L2_V110_PARAMS]) - ?SIZEOF_IISDN_L2_SS7_PARAMS,
	<<(Mtp2#l2_ss7_params.mode):?IISDNu8bit,
			(Mtp2#l2_ss7_params.variant):?IISDNu8bit,
			0:?IISDNu16bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit,
			0:Pad/integer-unit:8>>.

%% @type l2_v110_params().  Layer 2 V.110 parameters.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>mode</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>ebits</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>flow_control</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>nine_byte_rx_frames</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>num_tx_idle_frames</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>max_rx_frame_size</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>stale_rx_data_timer</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>filter_status_messages</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec (L2V110ParamsRec) -> L2V110ParamsBin
%% 	L2V110ParamsRec = l2_v110_params()
%% 	L2V110ParamsBin = binary()
%%
l2_v110_params(V110) when is_record(V110, l2_v110_params) ->
	Pad = lists:max([?SIZEOF_IISDN_L2_LAP_PARAMS,
			?SIZEOF_IISDN_L2_UDPIP_PARAMS,
			?SIZEOF_IISDN_L2_TCPIP_PARAMS,
			?SIZEOF_IISDN_L2_DPNSS_PARAMS,
			?SIZEOF_IISDN_L2_SS7_PARAMS,
			?SIZEOF_IISDN_L2_V110_PARAMS]) - ?SIZEOF_IISDN_L2_V110_PARAMS,
	<<(V110#l2_v110_params.mode):?IISDNu8bit,
			(V110#l2_v110_params.ebits):?IISDNu8bit,
			(V110#l2_v110_params.flow_control):?IISDNu8bit,
			(V110#l2_v110_params.nine_byte_rx_frames):?IISDNu8bit,
			(V110#l2_v110_params.num_tx_idle_frames):?IISDNu16bit,
			(V110#l2_v110_params.max_rx_frame_size):?IISDNu16bit,
			(V110#l2_v110_params.stale_rx_data_timer):?IISDNu16bit,
			(V110#l2_v110_params.filter_status_messages):?IISDNu8bit,
			0:?IISDNu8bit, 0:Pad/integer-unit:8>>.
	
%% @type l2_lap_consts().  Layer 2 LAP constants.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>t200</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>t201</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>t202</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>t203</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>n200</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>n201</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>n202</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>k</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec (L2LapConstsRec) -> L2LapConstsBin
%% 	L2LapConstsRec = l2_lap_consts()
%% 	L2LapConstsBin = binary()
%%
l2_lap_consts(L2) when is_record(L2, l2_lap_consts) ->
	Pad = lists:max([?SIZEOF_IISDN_L2_LAP_CONSTS,
			?SIZEOF_IISDN_L2_IP_CONSTS,
			?SIZEOF_IISDN_L2_DPNSS_CONSTS,
			?SIZEOF_IISDN_L2_SS7_CONSTS]) - ?SIZEOF_IISDN_L2_LAP_CONSTS,
	<<(L2#l2_lap_consts.t200):?IISDNu16bit,
			(L2#l2_lap_consts.t201):?IISDNu16bit,
			(L2#l2_lap_consts.t202):?IISDNu16bit,
			(L2#l2_lap_consts.t203):?IISDNu16bit,
			(L2#l2_lap_consts.n200):?IISDNu16bit,
			(L2#l2_lap_consts.n201):?IISDNu16bit,
			(L2#l2_lap_consts.n202):?IISDNu16bit,
			(L2#l2_lap_consts.k):?IISDNu16bit,
			0:Pad/integer-unit:8>>.

%% @type l2_ip_consts().  Layer 2 IP constants.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>no_dhcp</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>ipaddr</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>gwaddr</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>subnet_mask</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec (L2IpConsts) -> L2IpConsts
%% 	L2IpConsts = l2_ip_consts() | binary()
%%
l2_ip_consts(Ip) when is_record(Ip, l2_ip_consts) ->
	Pad = lists:max([?SIZEOF_IISDN_L2_LAP_CONSTS,
			?SIZEOF_IISDN_L2_IP_CONSTS,
			?SIZEOF_IISDN_L2_DPNSS_CONSTS,
			?SIZEOF_IISDN_L2_SS7_CONSTS]) - ?SIZEOF_IISDN_L2_IP_CONSTS,
	<<(Ip#l2_ip_consts.no_dhcp):?IISDNu8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit,
			(Ip#l2_ip_consts.ipaddr):?IISDNu32bit,
			(Ip#l2_ip_consts.gwaddr):?IISDNu32bit,
			(Ip#l2_ip_consts.subnet_mask):?IISDNu32bit,
			0:Pad/integer-unit:8>>;
l2_ip_consts(Ip) when is_binary(Ip) ->
	<<No_dhcp:?IISDNu8bit, _:?IISDNu8bit, _:?IISDNu8bit, _:?IISDNu8bit,
			Ipaddr:?IISDNu32bit, Gwaddr:?IISDNu32bit,
			Subnet_mask:?IISDNu32bit, _Rest/binary>> = Ip,
	#l2_ip_consts{no_dhcp = No_dhcp, ipaddr = Ipaddr,
			gwaddr = Gwaddr, subnet_mask = Subnet_mask}.

%% @type l2_dpnss_consts().  Layer 2 DPNSS constants.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>nl</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>nt1</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>nt2</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec (L2DpnssConstsRec) -> L2DpnssConstsBin
%% 	L2DpnssConstsRec = l2_dpnss_consts()
%% 	L2DpnssConstsBin = binary()
%%
l2_dpnss_consts(Dpnss) when is_record(Dpnss, l2_dpnss_consts) ->
	Pad = lists:max([?SIZEOF_IISDN_L2_LAP_CONSTS,
			?SIZEOF_IISDN_L2_IP_CONSTS,
			?SIZEOF_IISDN_L2_DPNSS_CONSTS,
			?SIZEOF_IISDN_L2_SS7_CONSTS]) - ?SIZEOF_IISDN_L2_DPNSS_CONSTS,
	<<(Dpnss#l2_dpnss_consts.nl):?IISDNu16bit,
			0:?IISDNu16bit,
			(Dpnss#l2_dpnss_consts.nt1):?IISDNu32bit,
			(Dpnss#l2_dpnss_consts.nt2):?IISDNu32bit,
			0:Pad/integer-unit:8>>.

%% @type l2_ss7_consts().  Layer 2 SS7 constants.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>t1</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>t2</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>t3</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>t4n</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>t4e</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>t5</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>t6</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>t7</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec (L2Ss7ConstsRec) -> L2Ss7ConstsRecBin
%% 	L2Ss7ConstsRec = l2_ss7_consts()
%% 	L2Ss7ConstsRecBin = binary()
%%
l2_ss7_consts(Mtp2) when is_record(Mtp2, l2_ss7_consts) ->
	Pad = lists:max([?SIZEOF_IISDN_L2_LAP_CONSTS,
			?SIZEOF_IISDN_L2_IP_CONSTS,
			?SIZEOF_IISDN_L2_DPNSS_CONSTS,
			?SIZEOF_IISDN_L2_SS7_CONSTS]) - ?SIZEOF_IISDN_L2_SS7_CONSTS,
	<<(Mtp2#l2_ss7_consts.t1):?IISDNu16bit,
			(Mtp2#l2_ss7_consts.t2):?IISDNu16bit,
			(Mtp2#l2_ss7_consts.t3):?IISDNu16bit,
			(Mtp2#l2_ss7_consts.t4n):?IISDNu16bit,
			(Mtp2#l2_ss7_consts.t4e):?IISDNu16bit,
			(Mtp2#l2_ss7_consts.t5):?IISDNu16bit,
			(Mtp2#l2_ss7_consts.t6):?IISDNu16bit,
			(Mtp2#l2_ss7_consts.t7):?IISDNu16bit,
			0:Pad/integer-unit:8>>.

%% @type level3().  Layer 3 configuration.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>l3_mode</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>cnfg</tt></dt><dd><tt>q931() | bonding_data()
%% 				| x25() | pm() | relay() | dpnss() | dass() 
%% 				| q933a</tt></dd>
%% 	</dl>
%%
%% @spec (Level3Rec) -> Level3Bin
%% 	Level3Rec = level3()
%% 	Level3Bin = binary()
%%
level3(L3) when is_record(L3, level3),
		is_record(L3#level3.cnfg, q931) ->
	Bin = q931(L3#level3.cnfg),
	level3(L3#level3{cnfg = Bin});
level3(L3) when is_record(L3, level3),
		is_record(L3#level3.cnfg, bonding_data) ->
	Bin = bonding_data(L3#level3.cnfg),
	level3(L3#level3{cnfg = Bin});
level3(L3) when is_record(L3, level3),
		is_record(L3#level3.cnfg, x25) ->
	Bin = x25(L3#level3.cnfg),
	level3(L3#level3{cnfg = Bin});
level3(L3) when is_record(L3, level3),
		is_record(L3#level3.cnfg, pm) ->
	Bin = pm(L3#level3.cnfg),
	level3(L3#level3{cnfg = Bin});
level3(L3) when is_record(L3, level3),
		is_record(L3#level3.cnfg, relay) ->
	Bin = relay(L3#level3.cnfg),
	level3(L3#level3{cnfg = Bin});
level3(L3) when is_record(L3, level3),
		is_record(L3#level3.cnfg, dpnss) ->
	Bin = dpnss(L3#level3.cnfg),
	level3(L3#level3{cnfg = Bin});
level3(L3) when is_record(L3, level3),
		is_record(L3#level3.cnfg, dass) ->
	Bin = dass(L3#level3.cnfg),
	level3(L3#level3{cnfg = Bin});
level3(L3) when is_record(L3, level3),
		is_record(L3#level3.cnfg, q933a) ->
	Bin = q933a(L3#level3.cnfg),
	level3(L3#level3{cnfg = Bin});
level3(L3) when is_record(L3, level3),
		is_binary(L3#level3.cnfg) ->
	<<(L3#level3.l3_mode):?IISDNu8bit, 0:?IISDNu8bit,
			0:?IISDNu16bit, (L3#level3.cnfg)/binary>>.


%% @type q931().  Q.931 call control configuration.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%%			<dt><tt>switch_type</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>variant</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>call_filtering</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>q931_timers</tt></dt><dd><tt>q931_timers()</tt></dd>
%%			<dt><tt>b_channel_service_state</tt></dt><dd><tt>[integer()]</tt></dd>
%%			<dt><tt>nfas</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>e1_30_bchan</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>basic_rate</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>net_side_emul</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>b_chan_negot</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>proc_on_exclusv</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>chanid_slot_map</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>sprs_chanid_callproc</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>no_chanid_callproc</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>append_raw_qmsg</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>ccitt_mode</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>raw_qmsg</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>no_ie_errcheck</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>user_ie_encode</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>user_ie_encode</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>overlap_rcv</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>send_l3l4_callproc</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>sending_cmplt</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>require_send_complete</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>report_incoming_callproc</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>no_tx_conn_ack</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>no_rx_conn_ack</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>sprs_chanid_setupack</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>no_chanid_setupack</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>no_canned_spid_rej</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>call_reject_notify</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>advice_of_charge</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>message_segmentation</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>no_bc_user_info</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>incoming_call_slot_map</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>release_complete_control</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>primary_lapdid</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>backup_lapdid</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>primary_ifnum</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>backup_ifnum</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>backup_control</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>backup_control</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>spid_len</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>spid_1_len</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>dn_len</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>dn_1_len</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>spid</tt></dt><dd><tt>[integer()]</tt></dd>
%%			<dt><tt>spid_1</tt></dt><dd><tt>[integer()]</tt></dd>
%%			<dt><tt>dn</tt></dt><dd><tt>[integer()]</tt></dd>
%%			<dt><tt>dn_1</tt></dt><dd><tt>[integer()]</tt></dd>
%%			<dt><tt>chan_id_high_bit</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>att_cust_bri_ekts</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>subscribe_connack</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>suppress_auto_spid</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>accept_all_bri_calls</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec(Q931Rec) -> Q931Bin
%% 	Q931Rec = q931()
%% 	Q931Bin = binary()
%%
q931(Q931) when is_record(Q931, q931) ->
	Q931_Timers = q931_timers(Q931#q931.q931_timers),
	Digit32 = fun(Digit, Bin) -> <<Bin/binary, Digit:?IISDNu32bit>> end,
	B_channel_service_state = lists:foldl(Digit32, <<>>,
			Q931#q931.b_channel_service_state),
	Digit8 = fun(Digit, Bin) -> <<Bin/binary, Digit:?IISDNu8bit>> end,
	Spid = lists:foldl(Digit8, <<>>, Q931#q931.spid),
	Spid_1 = lists:foldl(Digit8, <<>>, Q931#q931.spid_1),
	Dn = lists:foldl(Digit8, <<>>, Q931#q931.dn),
	Dn_1 = lists:foldl(Digit8, <<>>, Q931#q931.dn_1),
	Pad = lists:max([?SIZEOF_IISDN_Q931_CNFG,
			?SIZEOF_IISDN_BONDING_DATA,
			?SIZEOF_IISDN_X25_CONFIG,
			?SIZEOF_IISDN_PM_CONFIG,
			?SIZEOF_IISDN_RELAY_CONFIG,
			?SIZEOF_IISDN_DPNSSCC_CONFIG,
			?SIZEOF_IISDN_DASSCC_CONFIG,
			?SIZEOF_IISDN_Q933A_CONFIG]) - ?SIZEOF_IISDN_Q931_CNFG,
	<<(Q931#q931.switch_type):?IISDNu16bit,
			(Q931#q931.variant):?IISDNu16bit,
			(Q931#q931.call_filtering):?IISDNu32bit,
			Q931_Timers/binary,
			B_channel_service_state/binary,
			(Q931#q931.nfas):?IISDNu8bit,
			(Q931#q931.e1_30_bchan):?IISDNu8bit,
			(Q931#q931.basic_rate):?IISDNu8bit,
			(Q931#q931.net_side_emul):?IISDNu8bit,
			(Q931#q931.b_chan_negot):?IISDNu8bit,
			(Q931#q931.proc_on_exclusv):?IISDNu8bit,
			(Q931#q931.chanid_slot_map):?IISDNu8bit,
			(Q931#q931.sprs_chanid_callproc):?IISDNu8bit,
			(Q931#q931.no_chanid_callproc):?IISDNu8bit,
			(Q931#q931.append_raw_qmsg):?IISDNu8bit,
			(Q931#q931.ccitt_mode):?IISDNu8bit,
			(Q931#q931.raw_qmsg):?IISDNu8bit,
			(Q931#q931.no_ie_errcheck):?IISDNu8bit,
			(Q931#q931.user_ie_encode):?IISDNu8bit,
			(Q931#q931.overlap_rcv):?IISDNu8bit,
			(Q931#q931.send_l3l4_callproc):?IISDNu8bit,
			(Q931#q931.sending_cmplt):?IISDNu8bit,
			(Q931#q931.require_send_complete):?IISDNu8bit,
			(Q931#q931.report_incoming_callproc):?IISDNu8bit,
			(Q931#q931.no_tx_conn_ack):?IISDNu8bit,
			(Q931#q931.no_rx_conn_ack):?IISDNu8bit,
			(Q931#q931.sprs_chanid_setupack):?IISDNu8bit,
			(Q931#q931.no_chanid_setupack):?IISDNu8bit,
			(Q931#q931.no_canned_spid_rej):?IISDNu8bit,
			(Q931#q931.call_reject_notify):?IISDNu8bit,
			(Q931#q931.advice_of_charge):?IISDNu8bit,
			(Q931#q931.message_segmentation):?IISDNu8bit,
			(Q931#q931.no_bc_user_info):?IISDNu8bit,
			(Q931#q931.incoming_call_slot_map):?IISDNu8bit,
			(Q931#q931.release_complete_control):?IISDNu8bit,
			(Q931#q931.primary_lapdid):?IISDNu8bit,
			(Q931#q931.backup_lapdid):?IISDNu8bit,
			0:?IISDNu16bit,
			(Q931#q931.primary_ifnum):?IISDNu8bit,
			(Q931#q931.backup_ifnum):?IISDNu8bit,
			(Q931#q931.backup_control):?IISDNu8bit,
			(Q931#q931.spid_len):?IISDNu8bit,
			(Q931#q931.spid_1_len):?IISDNu8bit,
			(Q931#q931.dn_len):?IISDNu8bit,
			(Q931#q931.dn_1_len):?IISDNu8bit,
			Spid/binary, Spid_1/binary, Dn/binary, Dn_1/binary,
			(Q931#q931.chan_id_high_bit):?IISDNu8bit,
			(Q931#q931.att_cust_bri_ekts):?IISDNu8bit,
			(Q931#q931.subscribe_connack):?IISDNu8bit,
			(Q931#q931.suppress_auto_spid):?IISDNu8bit,
			(Q931#q931.accept_all_bri_calls):?IISDNu8bit,
			0:?IISDNu16bit, 0:Pad/integer-unit:8>>.

%% @type x25().  X.25 configuration.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%%			<dt><tt>cfg_msk</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>t10</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>t11</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>t12</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>t13</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>t28</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>p</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>w</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>max_clr_retry</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>max_svcs</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>max_pvcs</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec(X25Rec) -> X25Bin
%% 	X25Rec = x25()
%% 	X25Bin = binary()
%%
x25(X25) when is_record(X25, x25) ->
	Pad = lists:max([?SIZEOF_IISDN_Q931_CNFG,
			?SIZEOF_IISDN_BONDING_DATA,
			?SIZEOF_IISDN_X25_CONFIG,
			?SIZEOF_IISDN_PM_CONFIG,
			?SIZEOF_IISDN_RELAY_CONFIG,
			?SIZEOF_IISDN_DPNSSCC_CONFIG,
			?SIZEOF_IISDN_DASSCC_CONFIG,
			?SIZEOF_IISDN_Q933A_CONFIG]) - ?SIZEOF_IISDN_X25_CONFIG,
	<<(X25#x25.cfg_msk):?IISDNu32bit,
			(X25#x25.t10):?IISDNu16bit,
			(X25#x25.t11):?IISDNu16bit,
			(X25#x25.t12):?IISDNu16bit,
			(X25#x25.t13):?IISDNu16bit,
			(X25#x25.t28):?IISDNu16bit,
			(X25#x25.p):?IISDNu16bit,
			(X25#x25.w):?IISDNu8bit,
			(X25#x25.max_clr_retry):?IISDNu8bit,
			(X25#x25.max_svcs):?IISDNu8bit,
			(X25#x25.max_pvcs):?IISDNu8bit,
			0:Pad/integer-unit:8>>.

%% @type bonding_data().  Bandwidth On Demand configuration.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%%			<dt><tt>mode</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>destination</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>num_tx_buf</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>num_rx_buf</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>data_channel</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>txinit</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>txadd01</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>txfa</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>txdisc</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>tcid</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>tanull</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>channels</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>directory</tt></dt><dd><tt>[integer()]</tt></dd>
%% 	</dl>
%%
%% @spec(BondingDataRec) -> BondingDataBin
%% 	BondingDataRec = bonding_data()
%% 	BondingDataBin = binary()
%%
bonding_data(Bond) when is_record(Bond, bonding_data) ->
	Digit32 = fun(Digit, Bin) -> <<Bin/binary, Digit:?IISDNu32bit>> end,
	Directory = lists:foldl(Digit32, <<>>, Bond#bonding_data.directory),
	Pad = lists:max([?SIZEOF_IISDN_Q931_CNFG,
			?SIZEOF_IISDN_BONDING_DATA,
			?SIZEOF_IISDN_X25_CONFIG,
			?SIZEOF_IISDN_PM_CONFIG,
			?SIZEOF_IISDN_RELAY_CONFIG,
			?SIZEOF_IISDN_DPNSSCC_CONFIG,
			?SIZEOF_IISDN_DASSCC_CONFIG,
			?SIZEOF_IISDN_Q933A_CONFIG]) - ?SIZEOF_IISDN_BONDING_DATA,
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
			Directory/binary, 0:Pad/integer-unit:8>>.
	
%% @type pm().  Performance Monitoring configuration.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%%			<dt><tt>mode</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>carrier</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>fdl_alert</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>equipmentid</tt></dt><dd><tt>[integer()]</tt></dd>
%%			<dt><tt>locationid</tt></dt><dd><tt>[integer()]</tt></dd>
%%			<dt><tt>frameid</tt></dt><dd><tt>[integer()]</tt></dd>
%%			<dt><tt>unitid</tt></dt><dd><tt>[integer()]</tt></dd>
%%			<dt><tt>facilityid</tt></dt><dd><tt>[integer()]</tt></dd>
%% 	</dl>
%%
%% @spec(PMRec) -> PMBin
%% 	PMRec = pm()
%% 	PMBin = binary()
%%
pm(PM) when is_record(PM, pm) ->
	Digit8 = fun(Digit, Bin) -> <<Bin/binary, Digit:?IISDNu8bit>> end,
	Equipmentid = lists:foldl(Digit8, <<>>, PM#pm.equipmentid),
	Locationid = lists:foldl(Digit8, <<>>, PM#pm.locationid),
	Frameid = lists:foldl(Digit8, <<>>, PM#pm.frameid),
	Unitid = lists:foldl(Digit8, <<>>, PM#pm.unitid),
	Facilityid = lists:foldl(Digit8, <<>>, PM#pm.facilityid),
	Pad = lists:max([?SIZEOF_IISDN_Q931_CNFG,
			?SIZEOF_IISDN_BONDING_DATA,
			?SIZEOF_IISDN_X25_CONFIG,
			?SIZEOF_IISDN_PM_CONFIG,
			?SIZEOF_IISDN_RELAY_CONFIG,
			?SIZEOF_IISDN_DPNSSCC_CONFIG,
			?SIZEOF_IISDN_DASSCC_CONFIG,
			?SIZEOF_IISDN_Q933A_CONFIG]) - ?SIZEOF_IISDN_PM_CONFIG,
	<<(PM#pm.mode):?IISDNu8bit, 
			(PM#pm.carrier):?IISDNu8bit,
			(PM#pm.fdl_alert):?IISDNu8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit,
			Equipmentid/binary, Locationid/binary, Frameid/binary,
			Unitid/binary, Facilityid/binary,
			0:Pad/integer-unit:8>>.

%% @type relay().  On board packet relay and routing configuration.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%%			<dt><tt>default_dest</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>default_dest_id</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>default_root_idx</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec(RelayRec) -> RelayBin
%% 	RelayRec = relay()
%% 	RelayBin = binary()
%%
relay(Relay) when is_record(Relay, relay) ->
	Pad = lists:max([?SIZEOF_IISDN_Q931_CNFG,
			?SIZEOF_IISDN_BONDING_DATA,
			?SIZEOF_IISDN_X25_CONFIG,
			?SIZEOF_IISDN_PM_CONFIG,
			?SIZEOF_IISDN_RELAY_CONFIG,
			?SIZEOF_IISDN_DPNSSCC_CONFIG,
			?SIZEOF_IISDN_DASSCC_CONFIG,
			?SIZEOF_IISDN_Q933A_CONFIG]) - ?SIZEOF_IISDN_RELAY_CONFIG,
	<<(Relay#relay.default_dest):?IISDNu8bit,
			(Relay#relay.default_dest_id):?IISDNu8bit,
			(Relay#relay.default_root_idx):?IISDNu8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit,
			0:Pad/integer-unit:8>>.

%% @type dpnss().  Digital Private Network Signaling System (DPNSS) configuration.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%%			<dt><tt>pbx_y</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>no_virtual_channels</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>dest_addr_len</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>b_channel_service_state</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>v_channel_service_state</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>t_i_msg</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>t_guard</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec(DpnssRec) -> DpnssBin
%% 	DpnssRec = dpnss()
%% 	DpnssBin = binary()
%%
dpnss(Dpnss) when is_record(Dpnss, dpnss) ->
	Pad = lists:max([?SIZEOF_IISDN_Q931_CNFG,
			?SIZEOF_IISDN_BONDING_DATA,
			?SIZEOF_IISDN_X25_CONFIG,
			?SIZEOF_IISDN_PM_CONFIG,
			?SIZEOF_IISDN_RELAY_CONFIG,
			?SIZEOF_IISDN_DPNSSCC_CONFIG,
			?SIZEOF_IISDN_DASSCC_CONFIG,
			?SIZEOF_IISDN_Q933A_CONFIG]) - ?SIZEOF_IISDN_DPNSSCC_CONFIG,
	<<(Dpnss#dpnss.pbx_y):?IISDNs8bit,
			(Dpnss#dpnss.no_virtual_channels):?IISDNs8bit,
			(Dpnss#dpnss.dest_addr_len):?IISDNs8bit,
			0:?IISDNu8bit,
			(Dpnss#dpnss.b_channel_service_state):?IISDNu32bit,
			(Dpnss#dpnss.v_channel_service_state):?IISDNu32bit,
			(Dpnss#dpnss.t_i_msg):?IISDNs32bit,
			(Dpnss#dpnss.t_guard):?IISDNs32bit,
			0:Pad/integer-unit:8>>.

%% @type dass().  Digital Access Signaling System (DASS) configuration.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%%			<dt><tt>b_channel_service_state</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>t_digit_racking</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>n_clear_retries</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec(DassRec) -> DassBin
%% 	DassRec = dass()
%% 	DassBin = binary()
%%
dass(Dass) when is_record(Dass, dass) ->
	Pad = lists:max([?SIZEOF_IISDN_Q931_CNFG,
			?SIZEOF_IISDN_BONDING_DATA,
			?SIZEOF_IISDN_X25_CONFIG,
			?SIZEOF_IISDN_PM_CONFIG,
			?SIZEOF_IISDN_RELAY_CONFIG,
			?SIZEOF_IISDN_DPNSSCC_CONFIG,
			?SIZEOF_IISDN_DASSCC_CONFIG,
			?SIZEOF_IISDN_Q933A_CONFIG]) - ?SIZEOF_IISDN_DASSCC_CONFIG,
	<<(Dass#dass.b_channel_service_state):?IISDNu32bit,
			(Dass#dass.t_digit_racking):?IISDNs32bit,
			(Dass#dass.n_clear_retries):?IISDNs8bit,
			0:?IISDNu8bit, 0:?IISDNu8bit, 0:?IISDNu8bit,
			0:Pad/integer-unit:8>>.

%% @type q933a().  Q.933 (Frame Realy) Annex A configuration.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%%			<dt><tt>network_side</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>n391</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>n392</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>n393</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>t391</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>t392</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec(Q933aRec) -> Q933aBin
%% 	Q933aRec = q933a()
%% 	Q933aBin = binary()
%%
q933a(Q933a) when is_record(Q933a, q933a) ->
	Pad = lists:max([?SIZEOF_IISDN_Q931_CNFG,
			?SIZEOF_IISDN_BONDING_DATA,
			?SIZEOF_IISDN_X25_CONFIG,
			?SIZEOF_IISDN_PM_CONFIG,
			?SIZEOF_IISDN_RELAY_CONFIG,
			?SIZEOF_IISDN_DPNSSCC_CONFIG,
			?SIZEOF_IISDN_DASSCC_CONFIG,
			?SIZEOF_IISDN_Q933A_CONFIG]) - ?SIZEOF_IISDN_Q933A_CONFIG,
	<<(Q933a#q933a.network_side):?IISDNu8bit,
			(Q933a#q933a.n391):?IISDNu8bit,
			(Q933a#q933a.n392):?IISDNu8bit,
			(Q933a#q933a.n393):?IISDNu8bit,
			(Q933a#q933a.t391):?IISDNu16bit,
			(Q933a#q933a.t392):?IISDNu16bit,
			0:Pad/integer-unit:8>>.


%% @type ena_proto_data().  Enable protocol configuration.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%%			<dt><tt>command</tt></dt><dd><tt>integer()</tt>
%% 				Send a command to a protocol.  Used only after a
%% 				protocol was previously successfully configured.</dd>
%%			<dt><tt>command_parameter</tt></dt><dd><tt>integer()</tt>
%% 				Command parameter.  Unused.</dd>
%%			<dt><tt>level1</tt></dt><dd><tt>level1()</tt>
%% 				Specifies the layer 1 configuration (e.g. HDLC).</dd>
%%			<dt><tt>level2</tt></dt><dd><tt>level2()</tt>
%% 				Specifies the layer 2 configuration (e.g. LAPD).  Optional.</dd>
%%			<dt><tt>level3</tt></dt><dd><tt>level3()</tt>
%% 				Specifies the layer 3 configuration (e.g. Q.931).  Optional.</dd>
%% 	</dl>
%%
%% @spec(EnaProtoDataRec) -> EnaProtoDataBin
%% 	EnaProtoDataRec = ena_proto_data()
%% 	EnaProtoDataBin = binary()
%%
ena_proto_data(Proto) when is_record(Proto, ena_proto_data),
		is_record(Proto#ena_proto_data.level1, level1) ->
	ena_proto_data(Proto#ena_proto_data{
			level1 = level1(Proto#ena_proto_data.level1)});
ena_proto_data(Proto) when is_record(Proto, ena_proto_data),
		is_record(Proto#ena_proto_data.level2, level2) ->
	ena_proto_data(Proto#ena_proto_data{
			level2 = level2(Proto#ena_proto_data.level2)});
ena_proto_data(Proto) when is_record(Proto, ena_proto_data),
		is_record(Proto#ena_proto_data.level3, level3) ->
	ena_proto_data(Proto#ena_proto_data{
			level3 = level3(Proto#ena_proto_data.level3)});
ena_proto_data(Proto) when is_record(Proto, ena_proto_data),
		is_binary(Proto#ena_proto_data.level1);
		is_binary(Proto#ena_proto_data.level2);
		is_binary(Proto#ena_proto_data.level3) ->
	<<(Proto#ena_proto_data.command):?IISDNu16bit,
			(Proto#ena_proto_data.command_parameter):?IISDNu16bit,
			(Proto#ena_proto_data.level1)/binary,
			(Proto#ena_proto_data.level2)/binary,
			(Proto#ena_proto_data.level3)/binary>>.

%% @type hardware_data().  A record which includes the following fields:
%% 	<dl>
%% 		<dt><tt>clocking</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>clocking2</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>enable_clocking2</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>netref_clocking</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>netref_rate</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>ctbus_mode</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>force_framer_init</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>tdm_rate</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>enable_8370_rliu_monitor</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>dbcount</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>enable_t810x_snap_mode</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>clk_status</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>line_data</tt></dt> <dd><tt>[line_data()]</tt></dd>
%% 		<dt><tt>csu</tt></dt> <dd><tt>[boolean()]</tt></dd>
%% 	</dl>
%%
%% @spec(HardwareData) -> HardwareData
%% 	HardwareData = hardware_data() | binary()

%%
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
hardware_data(HW) when is_record(HW, hardware_data), 
		is_list(HW#hardware_data.line_data) ->
	LtoBin = fun(Line, Bin) ->
				B = line_data(Line),
				<<Bin/binary, B/binary>>
			end,
	LD = lists:foldl(LtoBin, <<>>, HW#hardware_data.line_data),
	NewHW = HW#hardware_data{line_data=LD},
	hardware_data(NewHW);
hardware_data(HW) when is_binary(HW) ->
	Size_line = (?IISDN_MAX_LINES * ?SIZEOF_IISDN_LINE_DATA),
	Size_csu = (?IISDN_MAX_LINES * ?SIZEOF_IISDNu8bit),
	<<Clocking:?IISDNu8bit, Clocking2:?IISDNu8bit,
			Enable_clocking2:?IISDNu8bit, Netref_clocking:?IISDNu8bit,
			Netref_rate:?IISDNu8bit, Ctbus_mode:?IISDNu8bit,
			Force_framer_init:?IISDNu8bit, Tdm_rate:?IISDNu8bit,
			Enable_8370_rliu_monitor:?IISDNu8bit, Dbcount:?IISDNu8bit,
			Enable_t810x_snap_mode:?IISDNu8bit, Clk_status:?IISDNu8bit,
			LineData:Size_line/binary, Csu:Size_csu/binary, _Rest/binary>> = HW,
	U8toL = fun (_Iter, <<>>, List) -> List;
			(Iter, <<Digit:?IISDNu8bit, Rest/binary>>, Acc) ->
				Iter(Iter, Rest, Acc ++ [Digit])
			end,
	LinetoL = fun (_Iter, <<>>, List) -> List;
			(Iter, <<LD:?SIZEOF_IISDN_LINE_DATA/binary, Rest/binary>>, Acc) ->
				Iter(Iter, Rest, Acc ++ [line_data(LD)])
			end,
	#hardware_data{clocking=Clocking, clocking2=Clocking2,
			enable_clocking2=Enable_clocking2, 
			netref_clocking=Netref_clocking, netref_rate=Netref_rate,
			ctbus_mode=Ctbus_mode, force_framer_init=Force_framer_init,
			tdm_rate=Tdm_rate,
			enable_8370_rliu_monitor=Enable_8370_rliu_monitor,
			dbcount=Dbcount, enable_t810x_snap_mode=Enable_t810x_snap_mode,
			clk_status=Clk_status, line_data = LinetoL(LinetoL, LineData, []),
			csu = U8toL(U8toL, Csu, [])}.


%% @type line_data(). A record which includes the following fields:
%% 	<dl>
%% 		<dt><tt>framing</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>line_code</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>pm_mode</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>line_length</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>term</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>line_type</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>integrate_alarms</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>filter_unsolicited</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>filter_yellow</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>bri_l1mode</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>briL1_cmd</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>bri_loop</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>briL1_T3</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>briL1_T4</tt></dt> <dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec(LineData) -> LineData
%% 	LineData = line_data() | binary()
%%
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
			_Pad:?IISDNu8bit, Filter_yellow:?IISDNu8bit, Bri_l1mode:?IISDNu8bit,
			BriL1_cmd:?IISDNu8bit, Bri_loop:?IISDNu8bit, 
			Bril1_t3:?IISDNu8bit, Bril1_t4:?IISDNu16bit>> = LD,
	#line_data{framing=Framing, line_code=Line_code,
			pm_mode=Pm_mode, line_length=Line_length, term=Term,
			line_type=line_type(Line_type), integrate_alarms=Integrate_alarms,
			filter_unsolicited=Filter_unsolicited, 
			filter_yellow=Filter_yellow, bri_l1mode=Bri_l1mode,
			briL1_cmd=BriL1_cmd, bri_loop=Bri_loop,
			bril1_t3=Bril1_t3, bril1_t4=Bril1_t4}.

%% @type line_type().  t1 | t1_csu | pri_e1 | bri_u | bri_st | integer()
%%
%% @spec(LineType) -> LineType
%% 	LineType = line_type() | binary() | [line_type()]
%%
line_type(1) -> t1;
line_type(2) -> t1_csu;
line_type(3) -> pri_e1;
line_type(4) -> bri_u;
line_type(5) -> bri_st;
line_type(t1) -> 1;
line_type(t1_csu) -> 2;
line_type(pri_e1) -> 3;
line_type(bri_u) -> 4;
line_type(bri_st) -> 5;
line_type(Bin) when is_binary(Bin) ->
	line_type(Bin, []);
line_type(Other) -> Other.
line_type(<<>>, List) -> List;
line_type(<<Type:?IISDNu8bit, Rest/binary>>, List) ->
	line_type(Rest, List ++ [line_type(Type)]).
	
	
%% @type tsi_data().  A record which includes the following fields:
%% 	<dl>
%% 		<dt><tt>tsi_ack_enable</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>num_mappings</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>granularity</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>last</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>tsi_map</tt></dt> <dd><tt>[tsi_map()]</tt></dd>
%% 	</dl>
%%
%% @spec(TsiData) -> TsiData
%% 	TsiData = tsi_data() | binary()
%%
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
	tsi_data(TS#tsi_data{tsi_map = MAP});
tsi_data(TS) when is_binary(TS) ->
	<<TsiAckEnable:?IISDNu8bit,
			NumMappings:?IISDNu8bit,
			Granularity:?IISDNu8bit,
			Last:?IISDNu8bit,
			Map/binary>> = TS,
	MapList = tsi_data(NumMappings, Map, []),
	#tsi_data{tsi_ack_enable = TsiAckEnable,
			num_mappings = NumMappings,
			granularity = Granularity,
			last = Last,
			tsi_map = MapList}.
tsi_data(0, _, MapList) -> MapList;
tsi_data(_, <<>>, MapList) -> MapList;
tsi_data(N, <<Destination:?IISDNu16bit, Source:?IISDNu16bit,
		Rest/binary>>, MapList) ->
	tsi_data(N - 1, Rest, MapList ++ [tsi_map(<<Destination:?IISDNu16bit,
			Source:?IISDNu16bit>>)]).

%% @type tsi_map().  A unidirectional timeslot mapping.
%% 	<p>A record which contains the following fields:</p>
%% 	<dl>
%% 		<dt><tt>destination</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>source</tt></dt> <dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec(TsiMap) -> TsiMap
%% 	TsiMap = tsi_map() | binary()
%%
tsi_map(MAP) when is_record(MAP, tsi_map) ->
	<<(MAP#tsi_map.destination):?IISDNu16bit,
			(MAP#tsi_map.source):?IISDNu16bit>>;
tsi_map(MAP) when is_binary(MAP) ->
	<<Destination:?IISDNu16bit, Source:?IISDNu16bit>> = MAP,
	#tsi_map{destination = Destination, source = Source}.

%% @type q931_timers().  Q.931 timers configuration.
%% 	<p>A record which contains the following fields:</p>
%% 	<dl>
%% 		<dt><tt>t302</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>t305</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>t308</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>t313</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>t314</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>t316</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>t318</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>t319</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>t3m1</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>t321</tt></dt> <dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec (Q931TimersRec) -> Q931TimersBin
%% 	Q931TimersRec = q931_timers()
%% 	Q931TimersBin = binary()
%%
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

%% @type protocol_stat().  Protocol status.
%% 	<p>A record which contains the following fields:</p>
%% 	<dl>
%% 		<dt><tt>status</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>l2_state</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>l2_error</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>l2_errpt</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>b_channels</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>b_chan_req</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>txcount</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>rxcount</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>l2_detail</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>l2_detail_data</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>all_calls_dropped</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>num_q933a_pvcs</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>n_b_channels</tt></dt> <dd><tt>[integer()]</tt></dd>
%% 		<dt><tt>n_b_chan_req</tt></dt> <dd><tt>[integer()]</tt></dd>
%% 		<dt><tt>nfas_primary_dchan_status</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>nfas_backup_dchan_status</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>ethernet_speed</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>ethernet_mode</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>connectBPS</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>connectTyp</tt></dt> <dd><tt>integer()</tt></dd>
%% 		<dt><tt>ip</tt></dt> <dd><tt>l2_ip_consts()</tt></dd>
%% 		<dt><tt>q933a_pvcs</tt></dt> <dd><tt>[q933a_pvc_status()]</tt></dd>
%% 		<dt><tt>amf_status</tt></dt> <dd><tt>[integer()]</tt></dd>
%% 	</dl>
%%
%% @spec (ProtocolStateBin) -> ProtocolStateRec
%% 	ProtocolStateBin = binary()
%% 	ProtocolStateRec = protocol_stat()
%%
protocol_stat(P) when is_binary(P) ->
	Size_bchans = (?IISDN_NUM_DS1_INTERFACES * ?SIZEOF_IISDNu32bit),
	Size_q933a = ((?IISDN_MAX_VC_PER_CHAN - 1) * ?SIZEOF_IISDN_Q933A_PVC_STATUS),
	Size_amf = (4 * ?SIZEOF_IISDNu32bit),
	<<Status:?IISDNu8bit, L2_state:?IISDNu8bit,
			L2_error:?IISDNu8bit, L2_errpt:?IISDNu8bit, B_channels:?IISDNu32bit,
			B_chan_req:?IISDNu32bit, Txcount:?IISDNu32bit, Rxcount:?IISDNu32bit,
			L2_detail:?IISDNu16bit, L2_detail_data:?IISDNu16bit, 0:?IISDNu16bit,
			All_calls_dropped:?IISDNu8bit, Num_q933a_pvcs:?IISDNu8bit,
			Nbchans:Size_bchans/binary, Nbreq:Size_bchans/binary,
			Nfas_primary_dchan_status:?IISDNu8bit, 
			Nfas_backup_dchan_status:?IISDNu8bit,
			Ethernet_speed:?IISDNu8bit, Ethernet_mode:?IISDNu8bit,
			ConnectBPS:?IISDNu32bit, ConnectTyp:?IISDNu32bit,
			L2IpConsts:?SIZEOF_IISDN_L2_IP_CONSTS/binary,
			Q933a:Size_q933a/binary, Amf:Size_amf/binary, _Rest/binary>> = P,
	U32toL = fun (_Iter, <<>>, List) -> List;
			(Iter, <<Digit:?IISDNu32bit, Rest/binary>>, Acc) ->
				Iter(Iter, Rest, Acc ++ [Digit])
			end,
	N_b_channels = U32toL(U32toL, Nbchans, []),
	N_b_chan_req = U32toL(U32toL, Nbreq, []),
	QList = q933a_pvc_status(Q933a, []),
	Amf_status = U32toL(U32toL, Amf, []),
	#protocol_stat{status = Status, l2_state = L2_state, l2_error = L2_error,
			l2_errpt = L2_errpt, b_channels = B_channels, b_chan_req = B_chan_req,
			txcount = Txcount, rxcount = Rxcount, l2_detail = L2_detail, 
			l2_detail_data = L2_detail_data,
			all_calls_dropped = All_calls_dropped, num_q933a_pvcs = Num_q933a_pvcs,
			n_b_channels = N_b_channels, n_b_chan_req = N_b_chan_req,
			nfas_primary_dchan_status = Nfas_primary_dchan_status,
			nfas_backup_dchan_status = Nfas_backup_dchan_status,
			ethernet_speed = Ethernet_speed, ethernet_mode = Ethernet_mode,
			connectBPS = ConnectBPS, connectTyp = ConnectTyp,
			ip = l2_ip_consts(L2IpConsts), q933a_pvcs = QList,
			amf_status = Amf_status}.

%% @type q933a_pvc_status(). Q.933 (Frame Relay) Annex A PVC status.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>lli</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>status</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%% @spec (Q933aPvcStatusBin) -> Q933aPvcStatusRec
%% 	Q933aPvcStatusBin = binary()
%% 	Q933aPvcStatusRec = q933a_pvc_status()
%%
q933a_pvc_status(Q) when is_binary(Q) ->
	<<Lli:?IISDNu16bit, Status:?IISDNu8bit, _Pad:?IISDNu8bit>> = Q,
	#q933a_pvc_status{lli = Lli, status = Status}.
%% @hidden
q933a_pvc_status(<<>>, QList) -> QList;
q933a_pvc_status(<<Q:?SIZEOF_IISDN_Q933A_PVC_STATUS/binary, Rest/binary>>, QList) ->
	q933a_pvc_status(Rest, QList ++ [q933a_pvc_status(Q)]).
	
%% @type l2_stats().  Level 2 statitics counts for an LAP channel.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>iframe_tx</tt></dt><dd><tt>integer()</tt>  Count of IFRAME commands sent</dd>
%% 		<dt><tt>rr_cmd_tx</tt></dt><dd><tt>integer()</tt>  Count of RR commands sent</dd>
%% 		<dt><tt>rnr_cmd_tx</tt></dt><dd><tt>integer()</tt>  Count of RNR commands sent</dd>
%% 		<dt><tt>rej_cmd_tx</tt></dt><dd><tt>integer()</tt>  Count of REJ commands sent</dd>
%% 		<dt><tt>sabm_tx</tt></dt><dd><tt>integer()</tt>  Count of SABM commands sent</dd>
%% 		<dt><tt>sabme_tx</tt></dt><dd><tt>integer()</tt>  Count of SABME commands sent</dd>
%% 		<dt><tt>disc_tx</tt></dt><dd><tt>integer()</tt>  Count of DISC commands sent</dd>
%% 		<dt><tt>rr_rsp_tx</tt></dt><dd><tt>integer()</tt>  Count of RR responses sent</dd>
%% 		<dt><tt>rnr_rsp_tx</tt></dt><dd><tt>integer()</tt>  Count of RNR responses sent</dd>
%% 		<dt><tt>rej_rsp_tx</tt></dt><dd><tt>integer()</tt>  Count of REJ responses sent</dd>
%% 		<dt><tt>dm_tx</tt></dt><dd><tt>integer()</tt>  Count of DM responses sent</dd>
%% 		<dt><tt>ua_tx</tt></dt><dd><tt>integer()</tt>  Count of UA responses sent</dd>
%% 		<dt><tt>frmr_tx</tt></dt><dd><tt>integer()</tt>  Count of FRMR responses sent</dd>
%% 		<dt><tt>iframe_rx</tt></dt><dd><tt>integer()</tt>  Count of IFRAME commands received</dd>
%% 		<dt><tt>rr_cmd_rx</tt></dt><dd><tt>integer()</tt>  Count of RR commands received</dd>
%% 		<dt><tt>rnr_cmd_rx</tt></dt><dd><tt>integer()</tt>  Count of RNR commands received</dd>
%% 		<dt><tt>rej_cmd_rx</tt></dt><dd><tt>integer()</tt>  Count of REJ commands received</dd>
%% 		<dt><tt>sabm_rx</tt></dt><dd><tt>integer()</tt>  Count of SABM commands received</dd>
%% 		<dt><tt>sabme_rx</tt></dt><dd><tt>integer()</tt>  Count of SABME commands received</dd>
%% 		<dt><tt>disc_rx</tt></dt><dd><tt>integer()</tt>  Count of DISC commands received</dd>
%% 		<dt><tt>rr_rsp_rx</tt></dt><dd><tt>integer()</tt>  Count of RR responses received</dd>
%% 		<dt><tt>rnr_rsp_rx</tt></dt><dd><tt>integer()</tt>  Count of RNR responses received</dd>
%% 		<dt><tt>rej_rsp_rx</tt></dt><dd><tt>integer()</tt>  Count of REJ responses received</dd>
%% 		<dt><tt>dm_rx</tt></dt><dd><tt>integer()</tt>  Count of DM responses received</dd>
%% 		<dt><tt>ua_rx</tt></dt><dd><tt>integer()</tt>  Count of UA responses received</dd>
%% 		<dt><tt>frmr_rx</tt></dt><dd><tt>integer()</tt>  Count of FRMR responses received</dd>
%% 		<dt><tt>crc_errors</tt></dt><dd><tt>integer()</tt>  Count of CRC errors (RX)</dd>
%% 		<dt><tt>rcv_errors</tt></dt><dd><tt>integer()</tt>  Count of non-CRC errors (RX)</dd>
%% 		<dt><tt>retrans_cnt</tt></dt><dd><tt>integer()</tt>  Count of retransmitted IFRAMES</dd>
%% 		<dt><tt>poll_errors</tt></dt><dd><tt>integer()</tt>  Count of polls not responded to</dd>
%% 		<dt><tt>ui_tx</tt></dt><dd><tt>integer()</tt>  Count of UI frames transmitted</dd>
%% 		<dt><tt>ui_rx</tt></dt><dd><tt>integer()</tt>  Count of UI frames received</dd>
%% 	</dl>
%% @spec (L2StatsBin) -> L2StatsRec
%% 	L2StatsBin = binary()
%% 	L2StatsRec = l2_stats()
%%
l2_stats(L2StatsBin) when is_binary(L2StatsBin) ->
	<<IframeTx:?IISDNu16bit, RrCmdTx:?IISDNu16bit, RnrCmdTx:?IISDNu16bit, RejCmdTx:?IISDNu16bit, 
			SabmTx:?IISDNu16bit, SabmeTx:?IISDNu16bit, DiscTx:?IISDNu16bit, RrRspTx:?IISDNu16bit, 
			RnrRspTx:?IISDNu16bit, RejRspTx:?IISDNu16bit, DmTx:?IISDNu16bit, UaTx:?IISDNu16bit, 
			FrmrTx:?IISDNu16bit, IframeRx:?IISDNu16bit, RrCmdRx:?IISDNu16bit,
			RnrCmdRx:?IISDNu16bit, RejCmdRx:?IISDNu16bit, SabmRx:?IISDNu16bit, SabmeRx:?IISDNu16bit,
			DiscRx:?IISDNu16bit, RrRspRx:?IISDNu16bit, RnrRspRx:?IISDNu16bit, RejRspRx:?IISDNu16bit,
			DmRx:?IISDNu16bit, UaRx:?IISDNu16bit, FrmrRx:?IISDNu16bit, CrcErrors:?IISDNu16bit, 
			RcvErrors:?IISDNu16bit, RetransCnt:?IISDNu16bit, PollErrors:?IISDNu16bit,
			UiTx:?IISDNu16bit, UiRx:?IISDNu16bit, _Rest/binary>> = L2StatsBin,
	#l2_stats{iframe_tx=IframeTx, rr_cmd_tx=RrCmdTx, rnr_cmd_tx=RnrCmdTx, rej_cmd_tx=RejCmdTx,
			sabm_tx=SabmTx, sabme_tx=SabmeTx, disc_tx=DiscTx, rr_rsp_tx=RrRspTx, rnr_rsp_tx=RnrRspTx,
			 rej_rsp_tx=RejRspTx, dm_tx=DmTx, ua_tx=UaTx, frmr_tx=FrmrTx, iframe_rx=IframeRx,
			rr_cmd_rx=RrCmdRx, rnr_cmd_rx=RnrCmdRx, rej_cmd_rx=RejCmdRx, sabm_rx=SabmRx,
			sabme_rx=SabmeRx, disc_rx=DiscRx, rr_rsp_rx=RrRspRx, rnr_rsp_rx=RnrRspRx,
			rej_rsp_rx=RejRspRx, dm_rx=DmRx, ua_rx=UaRx, frmr_rx=FrmrRx, crc_errors=CrcErrors,
			rcv_errors=RcvErrors, retrans_cnt=RetransCnt, poll_errors=PollErrors,
			ui_tx=UiTx, ui_rx=UiRx}.

%% @type mtp2_stats().  Level 2 statitics counts for an MTP2 channel.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>msu_rx</tt></dt><dd><tt>integer()</tt>  Count of MSU received</dd>
%% 		<dt><tt>msu_tx</tt></dt><dd><tt>integer()</tt>  Count of MSU transmitted</dd>
%% 		<dt><tt>msu_rx_err</tt></dt><dd><tt>integer()</tt>  Count of MSU receive errors</dd>
%% 		<dt><tt>msu_re_tx</tt></dt><dd><tt>integer()</tt>  Count of MSU retransmitted</dd>
%% 		<dt><tt>neg_ack</tt></dt><dd><tt>integer()</tt>  Count of negatove acknowledgement</dd>
%% 		<dt><tt>octet_rx</tt></dt><dd><tt>integer()</tt>  Count of octets received</dd>
%% 		<dt><tt>octet_tx</tt></dt><dd><tt>integer()</tt>  Count of octets transmitted</dd>
%% 	</dl>
%% @spec (MTP2StatsBin) -> MTP2StatsRec
%% 	MTP2StatsBin = binary()
%% 	MTP2StatsRec = mtp2_stats()
%%
mtp2_stats(MTP2StatsBin) when is_binary(MTP2StatsBin) ->
	<<MsuRx:?IISDNu32bit, MsuTx:?IISDNu32bit, MsuRxErr:?IISDNu32bit, MsuReTx:?IISDNu32bit,
			NegAck:?IISDNu32bit, OctetRx:?IISDNu32bit, OctetTx:?IISDNu32bit, 
			_Rest/binary>> = MTP2StatsBin,
	#mtp2_stats{msu_rx=MsuRx, msu_tx=MsuTx, msu_rx_err=MsuRxErr, msu_re_tx=MsuReTx,
			neg_ack=NegAck, octet_rx=OctetRx, octet_tx=OctetTx}.

%% @type l2_mtp2_stats().  Level 2 statistics for an HDLC channel.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>mode</tt></dt><dd><tt>integer()</tt>  LAP=0, MTP2=1</dd>
%% 		<dt><tt>stats</tt></dt><dd><tt>integer()</tt>  l2_stats() or mtp2_stats()</dd>
%% 	</dl>
%% @spec (L2MTP2StatsBin) -> L2MTP2StatsRec
%%
l2_mtp2_stats(L2MTP2StatsBin) when is_binary(L2MTP2StatsBin) ->
	<<Mode:?IISDNu8bit, _:?IISDNu8bit, _:?IISDNu8bit, _:?IISDNu8bit,
			Stats/binary>> = L2MTP2StatsBin,
	#l2_mtp2_stats{mode = Mode, stats = Stats}.

%% @type line_status().  T1/E1 Line status.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>alarm_status</tt></dt><dd><tt>[alarm_status()]</tt></dd>
%% 		<dt><tt>l1_state</tt></dt><dd><tt>[integer()]</tt></dd>
%% 		<dt><tt>line_type</tt></dt><dd><tt>[integer()]</tt></dd>
%% 		<dt><tt>mph_msg</tt></dt><dd><tt>[integer()]</tt></dd>
%% 		<dt><tt>error_point</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec (LineStatusBin) -> LineStatusRec
%% 	LineStatusBin = binary()
%% 	LineStatusRec = line_status()
%%
line_status(LineStatusBin) when is_binary(LineStatusBin) ->
	Sizeof_AlarmStatus = (?IISDN_MAX_LINES * ?SIZEOF_IISDN_ALARM_STATUS),
	Sizeof_L1State = (?IISDN_MAX_LINES * ?SIZEOF_IISDNu8bit),
	Sizeof_LineType = (?IISDN_MAX_LINES * ?SIZEOF_IISDNs8bit),
	Sizeof_MphMsg = (?IISDN_MAX_LINES * ?SIZEOF_IISDNs8bit),
	<<Alarm_status:Sizeof_AlarmStatus/binary, L1State:Sizeof_L1State/binary,
			LineType:Sizeof_LineType/binary, MphMsg:Sizeof_MphMsg/binary,
			ErrorPoint:?IISDNu8bit, _Rest/binary>> = LineStatusBin,
	AlarmtoL = fun (_Iter, <<>>, List) -> List;
			(Iter, <<AS:?SIZEOF_IISDN_ALARM_STATUS/binary, Rest/binary>>, Acc) ->
				Iter(Iter, Rest, Acc ++ [alarm_status(AS)])
			end,
	U8toL = fun (_Iter, <<>>, List) -> List;
			(Iter, <<Digit:?IISDNu8bit, Rest/binary>>, Acc) ->
				Iter(Iter, Rest, Acc ++ [Digit])
			end,
	S8toL = fun (_Iter, <<>>, List) -> List;
			(Iter, <<Digit:?IISDNs8bit, Rest/binary>>, Acc) ->
				Iter(Iter, Rest, Acc ++ [Digit])
			end,
	#line_status{alarm_status = AlarmtoL(AlarmtoL, Alarm_status, []),
			l1_state = U8toL(U8toL, L1State, []),
			line_type = S8toL(S8toL, LineType, []),
			mph_msg = S8toL(S8toL, MphMsg, []), error_point = ErrorPoint}.

%% @type alarm_status().  T1/E1 Line alarm status.
%% 	<p>A record which includes the following fields:</p>
%% 	<dl>
%% 		<dt><tt>rcv_yellow</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>rcv_blue</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>rcv_red</tt></dt><dd><tt>integer()</tt></dd>
%% 		<dt><tt>snd_yellow</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%%
%% @spec (AlarmStatusBin) -> AlarmStatusRec
%% 	AlarmStatusBin = binary()
%% 	AlarmStatusRec = alarm_status()
%%
alarm_status(AlarmStatusBin) when is_binary(AlarmStatusBin) ->
	<<RcvYellow:?IISDNu8bit, RcvBlue:?IISDNu8bit, RcvRed:?IISDNu8bit,
			SndYellow:?IISDNu8bit>> = AlarmStatusBin,
	#alarm_status{rcv_yellow=RcvYellow, rcv_blue=RcvBlue, 
			rcv_red=RcvRed, snd_yellow=SndYellow}.

