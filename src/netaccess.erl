%%%---------------------------------------------------------------------
%%% @copyright Motivity Telecom Inc. 2001-2004
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
%%% @doc Main API of the Netaccess application.
%%%	<p>This module provides the main application programming interface
%%%	for the Netaccess application.  The application includes a
%%%	dynamically linked in device driver to the Netaccess series boards
%%%	from Brooktrout Technology.  This module is an Erlang binding to
%%%	the Instant ISDN&trade; Simple Message Interface (SMI) API.</p>
%%% @end
         
-module(netaccess).
-copyright('Copyright (c) 2001-2004 Motivity Telecom Inc.').
-vsn('0.1').
-author('vances@motivity.ca').

%% our published API functions
-export([start/0, start/1, start/2, start/3]).
-export([start_link/1, start_link/2, start_link/3]).
-export([stop/1]).
-export([open/1, close/1]).
-export([reset_board/1, boot/2]).
-export([get_version/1, get_driver_info/1]).
-export([set_hardware/2, req_hw_status/1]).
-export([board_id/1]).
-export([set_tsi/2, req_tsi_status/1]).
-export([enable_protocol/3]).

-include("pridrv.hrl").
-include("iisdn.hrl").


%%----------------------------------------------------------------------
%%  The API functions
%%----------------------------------------------------------------------

%% @spec () -> {ok, Server::pid()} | {error, Reason::term()}
%%
%% @doc Start the netacccess server.
%%
%% @see gen_server:start/4
%%
start() ->
	gen_server:start(netaccess_server, [?DEFAULT_BOARDNAME, 0], []).
	
%% @spec (BoardName::string()) -> {ok, Server::pid()} | {error, Reason::term()}
%%
%% @doc Start the netacccess server.
%% 	<p>e.g. <code>netaccess:start("/dev/pri0")</code></p>
%% @end
%%
%% @see gen_server:start/4
%%
start(BoardName) ->
	gen_server:start(netaccess_server, [BoardName, 0], []).
	
%% @spec (BoardName::string(), BoardNumber::integer()) -> {ok, Server::pid()} | {error, Reason::term()}
%%
%% @doc Start the netacccess server.
%% 	<p>e.g. <code>netaccess:start("/dev/pri0", 0)</code></p>
%% @end
%%
%% @see gen_server:start/4
%%
start(BoardName, BoardNumber) ->
	gen_server:start(netaccess_server, [BoardName, BoardNumber], []).
	
%% @spec (ServerName, BoardName::string(), BoardNumber::integer()) -> {ok, Server::pid()} | {error, Reason::term()}
%% 	ServerName = {local,Name} | {global,Name}
%% 	Name = Node = atom()
%%
%% @doc Start the netacccess server.
%%
%% @see gen_server:start/4
%%
start(ServerName, BoardName, BoardNumber) ->
	gen_server:start(ServerName, netaccess_server, [BoardName, BoardNumber], []).
	
%% @spec (BoardName::string()) -> {ok, Server::pid()} | {error, Reason::term()}
%%
%% @doc Start the netacccess server and link to the calling process.
%%
%% @see gen_server:start_link/4
%%
start_link(BoardName) ->
	gen_server:start_link(netaccess_server, [BoardName, 0], []).

%% @spec (BoardName::string(), BoardNumber::integer()) -> {ok, Server::pid()} | {error, Reason::term()}
%%
%% @doc Start the netacccess server and link to the calling process.
%% 	<p>e.g. <code>netaccess:start_link("/dev/pri0", 0)</code></p>
%% @end
%%
%% @see gen_server:start/4
%%
start_link(BoardName, BoardNumber) ->
	gen_server:start_link(netaccess_server, [BoardName, BoardNumber], []).
	
%% @spec (ServerName, BoardName::string()) -> {ok, Server::pid()} | {error, Reason::term()}
%% 	ServerName = {local,Name} | {global,Name}
%% 	Name = Node = atom()
%%
%% @doc Start the netacccess server and link to the calling process.
%%
%% @see gen_server:start_link/4
%%
start_link(ServerName, BoardName, BoardNumber) ->
	gen_server:start_link(ServerName, netaccess_server, [BoardName, BoardNumber], []).


%% @spec (ServerRef) -> ok
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = Node = atom()
%%
%% @doc Stop the netaccess server.
%%
stop(ServerRef) ->
	do_call(ServerRef, stop).


%% @spec (ServerRef) -> {ok, Channel::port()}
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = Node = atom()
%%
%% @doc Open a channel on the netaccess board.
%%
%% @see erlang:open_port/2
%%
open(ServerRef) ->
	do_call(ServerRef, {open, "netaccess_drv"}).

%% @spec (Channel::port()) -> true
%%
%% @doc Close a channel on a netaccess board.
%%
%% @see erlang:port_close/1
%%
close(Port) ->
	port_close(Port).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%% IOCTL calls                                                           %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec (ServerRef, BootFile) -> ok | {error, Reason:term()}
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = Node = atom()
%%		BootFile = binary() | string() | atom()
%%
%% @doc Boot an open netaccess board.
%%		<p>BootFile may be either the boot code itself in a binary
%%		or the file name of the boot file.</p>
%%
%%		<p>If the kernel is 64 bit the Erlang emulator as well
%%		as the netaccess driver must be compiled as 64 bit for
%%		this to work.  Otherwise it will return "badarg".</p>
%%
%% @see file:read_file/1
%%
boot(ServerRef, BootBin) when binary(BootBin) ->
	do_ioctl(ServerRef, {ioctl, ?BOOT_BOARD, BootBin}, 62000);
boot(ServerRef, Filename) when list(Filename) ->
	case catch file:read_file(Filename) of
		{ok, BootBin} ->
			boot(ServerRef, BootBin);
		Error ->
			Error	
	end.


%% @spec (ServerRef) -> ok
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = Node = atom()
%%
%% @doc Perform a reset on an open netaccess board.
%%
reset_board(ServerRef) ->
	do_ioctl(ServerRef, {ioctl, ?RESET_BOARD, 0}).


%% @spec (ServerRef) -> returns {ok, Version::string()} or {error, Reason::term()}
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = Node = atom()
%%
%% @doc Get software version string from an open netaccess board.
%%
get_version(ServerRef) ->
	do_ioctl(ServerRef, {ioctl, ?GET_VERSION, 0}, 2000).


%% @spec (ServerRef) -> {ok, driver_info()} | {error, Reason::term()}
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = Node = atom()
%%
%% @type driver_info(). A record which includes the following fields:
%%		<dl>
%%			<dt>board_type</dt> <dd><code>integer()</code></dd>
%%			<dt>hangup_on_red_alarm</dt> <dd><code>integer()</code></dd>
%%			<dt>flow_control_board</dt> <dd><code>integer()</code></dd>
%%			<dt>flow_control_wsrv</dt> <dd><code>integer()</code></dd>
%%			<dt>flow_control_rsrv</dt> <dd><code>integer()</code></dd>
%%			<dt>hdrops</dt> <dd><code>integer()</code></dd>
%%			<dt>sdrops</dt> <dd><code>integer()</code></dd>
%%			<dt>tx_msg_size</dt> <dd><code>integer()</code></dd>
%%			<dt>rx_msg_size</dt> <dd><code>integer()</code></dd>
%%			<dt>tx_num_bufs</dt> <dd><code>integer()</code></dd>
%%			<dt>rx_num_bufs</dt> <dd><code>integer()</code></dd>
%%			<dt>max_data_channels</dt> <dd><code>integer()</code>
%%					maximum number of data channels the driver can support</dd>
%%		</dl>
%%
%% @doc Get driver information from an open netaccess board.
%%
%%
get_driver_info(ServerRef) ->
	case do_ioctl(ServerRef, {ioctl, ?GET_DRIVER_INFO, 0}, 2000) of
		{ok, DriverInfo} ->
			{ok, pridrv:driver_info(DriverInfo)};
		Error ->
			Error	
	end.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%% SMI messages                                                          %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec (ServerRef) -> returns {ok, board_id()} or {error, Reason::term()}
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = Node = atom()
%%
%% @type board_id(). A record which includes the following fields:
%%		<dl>
%%			<dt>iisdn_ver</dt> <dd><code>string()</code></dd>
%%			<dt>banner</dt> <dd><code>string()</code></dd>
%%			<dt>date</dt> <dd><code>string()</code></dd>
%%			<dt>model</dt> <dd><code>string()</code></dd>
%%			<dt>rev</dt> <dd><code>string()</code></dd>
%%			<dt>board_type</dt> <dd><code>integer()</code></dd>
%%			<dt>num_lines</dt> <dd><code>integer()</code></dd>
%%			<dt>num_hdlc_chan</dt> <dd><code>integer()</code></dd>
%%			<dt>num_modem_chan</dt> <dd><code>integer()</code></dd>
%%			<dt>line_type</dt> <dd><code>[line_type()]</code></dd>
%%			<dt>kernel_ram_size</dt> <dd><code>integer()</code></dd>
%%			<dt>mezz_ram_size</dt> <dd><code>integer()</code></dd>
%%			<dt>num_bfio_devices</dt> <dd><code>integer()</code></dd>
%%		</dl>
%%
%% @type line_type() [t1 | t1_csu | pri_e1 | bri_u | bri_st] 
%%
%% @doc Get board identification.
%%
board_id(ServerRef) ->
	L4L3_rec = #l4_to_l3{msgtype = ?L4L3mREQ_BOARD_ID},
	do_call(ServerRef, {'L4L3m', L4L3_rec, <<>>}).


%% @spec (ServerRef, hardware_data()) -> ok
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = Node = atom()
%%
%% @doc Set hardware settings on a board.
%%
set_hardware(ServerRef, Data) when is_record(Data, hardware_data) ->
	L4L3_rec = #l4_to_l3{msgtype = ?L4L3mSET_HARDWARE, data = Data},
	do_cast(ServerRef, {'L4L3m', L4L3_rec, <<>>}).


%% @spec (ServerRef) -> {ok, hardware_data()} | {error, Reason::term()}
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = Node = atom()
%%
%% @type hardware_data().  A record which includes the following fields:
%% 	<dl>
%% 		<dt>clocking</dt> <dd><code>integer()</code></dd>
%% 		<dt>clocking2</dt> <dd><code>integer()</code></dd>
%% 		<dt>enable_clocking2</dt> <dd><code>integer()</code></dd>
%% 		<dt>netref_clocking</dt> <dd><code>integer()</code></dd>
%% 		<dt>netref_rate</dt> <dd><code>integer()</code></dd>
%% 		<dt>ctbus_mode</dt> <dd><code>integer()</code></dd>
%% 		<dt>force_framer_init</dt> <dd><code>integer()</code></dd>
%% 		<dt>tdm_rate</dt> <dd><code>integer()</code></dd>
%% 		<dt>enable_8370_rliu_monitor</dt> <dd><code>integer()</code></dd>
%% 		<dt>dbcount</dt> <dd><code>integer()</code></dd>
%% 		<dt>enable_t810x_snap_mode</dt> <dd><code>integer()</code></dd>
%% 		<dt>clk_status</dt> <dd><code>integer()</code></dd>
%% 		<dt>line_data</dt> <dd><code>[line_data()]</code></dd>
%% 		<dt>csu</dt> <dd><code>[boolean()]</code></dd>
%% 	</dl>
%%
%% @type line_data(). A record which includes the following fields:
%% 	<dl>
%% 		<dt>framing</dt> <dd><code>integer()</code></dd>
%% 		<dt>line_code</dt> <dd><code>integer()</code></dd>
%% 		<dt>pm_mode</dt> <dd><code>integer()</code></dd>
%% 		<dt>line_length</dt> <dd><code>integer()</code></dd>
%% 		<dt>term</dt> <dd><code>integer()</code></dd>
%% 		<dt>line_type</dt> <dd><code>integer()</code></dd>
%% 		<dt>integrate_alarms</dt> <dd><code>integer()</code></dd>
%% 		<dt>filter_unsolicited</dt> <dd><code>integer()</code></dd>
%% 		<dt>filter_yellow</dt> <dd><code>integer()</code></dd>
%% 		<dt>bri_l1mode</dt> <dd><code>integer()</code></dd>
%% 		<dt>briL1_cmd</dt> <dd><code>integer()</code></dd>
%% 		<dt>bri_loop</dt> <dd><code>integer()</code></dd>
%% 		<dt>briL1_T3</dt> <dd><code>integer()</code></dd>
%% 		<dt>briL1_T4</dt> <dd><code>integer()</code></dd>
%% 	</dl>
%%
%% @doc Query the hardware setup.
%%
req_hw_status(ServerRef) ->
	L4L3_rec = #l4_to_l3{msgtype = ?L4L3mREQ_HW_STATUS},
	do_call(ServerRef, {'L4L3m', L4L3_rec, <<>>}).
	
	
%% @spec (ServerRef, tsi_data()) -> ok
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = Node = atom()
%%
%% @type tsi_data().  A record which includes the following fields:
%% 	<dl>
%% 		<dt>tsi_ack_enable</dt> <dd><code>integer()</code></dd>
%% 		<dt>num_mappings</dt> <dd><code>integer()</code></dd>
%% 		<dt>granularity</dt> <dd><code>integer()</code></dd>
%% 		<dt>last</dt> <dd><code>integer()</code></dd>
%% 		<dt>tsi_map</dt> <dd><code>[tsi_map()]</code></dd>
%% 	</dl>
%%
%% @type tsi_map().  A record which contains the following fields:
%% 	<dl>
%% 		<dt>destination</dt> <dd><code>integer()</code></dd>
%% 		<dt>source</dt> <dd><code>integer()</code></dd>
%% 	</dl>
%%
%% @doc Create timeslot mappings.
%%
set_tsi(ServerRef, Data) ->
	L4L3_rec = #l4_to_l3{msgtype = ?L4L3mSET_TSI, data = Data},
	do_cast(ServerRef, {'L4L3m', L4L3_rec, <<>>}).


%% @spec (ServerRef) -> {ok, [tsi_data()]} | {error, Reason::term()}
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = Node = atom()
%%
%% @doc Retrieve the timeslot mappings.
%%
req_tsi_status(ServerRef) -> 
	L4L3_rec = #l4_to_l3{msgtype = ?L4L3mREQ_TSI_STATUS},
	do_call(ServerRef, {'L4L3m', L4L3_rec, <<>>}).


%% @spec (Channel::port(), LapdId::integer(), ena_proto_data()) ->
%%
%% @type ena_proto_data().  A record which includes the following fields:
%% 	<dl>
%% 	</dl>
%%
%% @doc Specifies and enables layer 1, 2 & 3 processing on an open channel.
%%
enable_protocol(Port, LapdId, Data) ->
	L4L3_rec = #l4_to_l3{lapdid = LapdId,
			msgtype = ?L4L3mENABLE_PROTOCOL, data = Data},
	L4L3_bin = iisdn:l4_to_l3(L4L3_rec),
	port_command(Port, L4L3_bin).
	
	
%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

do_ioctl(ServerRef, Request) ->
	do_ioctl(ServerRef, Request, 30000).
do_ioctl(ServerRef, Request, Timeout) ->
	gen_server:call(ServerRef, Request, Timeout).

do_call(ServerRef, Request) ->
	gen_server:call(ServerRef, Request).

do_cast(ServerRef, Request) ->
	gen_server:cast(ServerRef, Request).

