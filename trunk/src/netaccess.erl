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
%%% @doc Main API of the Netaccess application.
%%%	<p>This module provides the main application programming interface
%%%	for the Netaccess application.  The application includes a
%%%	dynamically linked in device driver to the Netaccess series boards
%%%	from Brooktrout Technology.  This module is an Erlang binding to
%%%	the Instant ISDN &#153; Simple Message Interface (SMI) API.</p>
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

%% @spec () -> {ok, Server} | {error, Reason}
%%		Server = pid()
%%		Reason = term()
%%
%% @doc Start the netacccess server.
%%
%% @see gen_server:start/4
%%
start() ->
	gen_server:start(netaccess_server, [?DEFAULT_BOARDNAME, 0], []).
	
%% @spec (BoardName) -> {ok, Server} | {error, Reason}
%% 	BoardName = string()
%%		Server = pid()
%%		Reason = term()
%%
%% @doc Start the netacccess server.
%% 	<p>e.g. <code>netaccess:start("/dev/pri0")</code></p>
%% @end
%%
%% @see gen_server:start/4
%%
start(BoardName) ->
	gen_server:start(netaccess_server, [BoardName, 0], []).
	
%% @spec (BoardName, BoardNumber) -> {ok, Server} | {error, Reason}
%% 	BoardName = string()
%% 	BoardNumber = integer()
%%		Server = pid()
%%		Reason = term()
%%
%% @doc Start the netacccess server.
%% 	<p>e.g. <code>netaccess:start("/dev/pri0", 0)</code></p>
%% @end
%%
%% @see gen_server:start/4
%%
start(BoardName, BoardNumber) ->
	gen_server:start(netaccess_server, [BoardName, BoardNumber], []).
	
%% @spec (ServerName, BoardName, BoardNumber) -> {ok, Server} | {error, Reason}
%% 	ServerName = {local,Name} | {global,Name}
%% 	BoardName = string()
%% 	BoardNumber = integer()
%% 	Name = atom()
%% 	Node = atom()
%%		Server = pid()
%%		Reason = term()
%%
%% @doc Start the netacccess server.
%%
%% @see gen_server:start/4
%%
start(ServerName, BoardName, BoardNumber) ->
	gen_server:start(ServerName, netaccess_server, [BoardName, BoardNumber], []).
	
%% @spec (BoardName) -> {ok, Server} | {error, Reason}
%% 	BoardName = string()
%%		Server = pid()
%%		Reason = term()
%%
%% @doc Start the netacccess server and link to the calling process.
%%
%% @see gen_server:start_link/4
%%
start_link(BoardName) ->
	gen_server:start_link(netaccess_server, [BoardName, 0], []).

%% @spec (BoardName, BoardNumber) -> {ok, Server} | {error, Reason}
%% 	BoardName = string()
%% 	BoardNumber = integer()
%%		Server = pid()
%%		Reason = term()
%%
%% @doc Start the netacccess server and link to the calling process.
%% 	<p>e.g. <code>netaccess:start_link("/dev/pri0", 0)</code></p>
%% @end
%%
%% @see gen_server:start/4
%%
start_link(BoardName, BoardNumber) ->
	gen_server:start_link(netaccess_server, [BoardName, BoardNumber], []).
	
%% @spec (ServerName, BoardName, BoardNumber) -> {ok, Server} | {error, Reason}
%% 	ServerName = {local,Name} | {global,Name}
%% 	BoardName = string()
%% 	BoardNumber = integer()
%% 	Name = atom()
%% 	Node = atom()
%%		Server = pid()
%%		Reason = term()
%%
%% @doc Start the netacccess server and link to the calling process.
%%
%% @see gen_server:start_link/4
%%
start_link(ServerName, BoardName, BoardNumber) ->
	gen_server:start_link(ServerName, netaccess_server, [BoardName, BoardNumber], []).


%% @spec (ServerRef) -> ok
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%%		Server = pid()
%%		Reason = term()
%%
%% @doc Stop the netaccess server.
%%
stop(ServerRef) ->
	do_call(ServerRef, stop).


%% @spec (ServerRef) -> Channel
%% 	ServerRef = Name | {Name, Node} | pid()
%% 	Name = atom()
%% 	Node = atom()
%%		Channel = port()
%%
%% @doc Open a channel on a netaccess board.
%% 	<p><b>Note:</b>  It is not possible to open a channel on a remote node.</p>
%%
%% @see erlang:open_port/2
%%
open(ServerRef) ->
	do_call(ServerRef, open).

%% @spec (Channel) -> true
%% 	Channel = port()
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

%% @spec (ServerRef, BootFile) -> ok | {error, Reason}
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%%		BootFile = binary() | string() | atom()
%%		Reason = term()
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
%% 	Name = atom()
%% 	Node = atom()
%%
%% @doc Perform a reset on an open netaccess board.
%%
reset_board(ServerRef) ->
	do_ioctl(ServerRef, {ioctl, ?RESET_BOARD, 0}).


%% @spec (ServerRef) -> {ok, Version} | {error, Reason}
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%%		Version = string()
%%		Reason = term()
%%
%% @doc Get software version string from an open netaccess board.
%%
get_version(ServerRef) ->
	do_ioctl(ServerRef, {ioctl, ?GET_VERSION, 0}, 2000).


%% @spec (ServerRef) -> {ok, DriverInfo} | {error, Reason}
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%% 	DriverInfo = driver_info()
%%		Reason = term()
%%
%% @see pridrv:driver_info()
%%
%% @doc Get driver information from an open netaccess board.
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

%% @spec (ServerRef) -> {ok, BoardId} | {error, Reason}
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%% 	BoardId = board_id()
%%		Reason = term()
%%
%% @doc Get board identification.
%%
board_id(ServerRef) ->
	L4L3_rec = #l4_to_l3{msgtype = ?L4L3mREQ_BOARD_ID},
	do_call(ServerRef, {'L4L3m', L4L3_rec, <<>>}).


%% @spec (ServerRef, HardWareData) -> ok
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%% 	HardwareData = hardware_data()
%%
%% @doc Set hardware settings on a board.
%%
set_hardware(ServerRef, Data) when is_record(Data, hardware_data) ->
	L4L3_rec = #l4_to_l3{msgtype = ?L4L3mSET_HARDWARE, data = Data},
	do_cast(ServerRef, {'L4L3m', L4L3_rec, <<>>}).


%% @spec (ServerRef) -> {ok, HardwareData} | {error, Reason}
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%% 	HardwareData = hardware_data()
%%		Reason = term()
%%
%% @doc Query the hardware setup.
%%
req_hw_status(ServerRef) ->
	L4L3_rec = #l4_to_l3{msgtype = ?L4L3mREQ_HW_STATUS},
	do_call(ServerRef, {'L4L3m', L4L3_rec, <<>>}).
	
	
%% @spec (ServerRef, TsiData) -> ok
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%% 	TsiData = tsi_data()
%%
%% @doc Create timeslot mappings.
%%
set_tsi(ServerRef, Data) ->
	L4L3_rec = #l4_to_l3{msgtype = ?L4L3mSET_TSI, data = Data},
	do_cast(ServerRef, {'L4L3m', L4L3_rec, <<>>}).


%% @spec (ServerRef) -> {ok, TsiDataList} | {error, Reason}
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%% 	TsiDataList = [tsi_data()]
%%		Reason = term()
%%
%% @doc Retrieve the timeslot mappings.
%%
req_tsi_status(ServerRef) -> 
	L4L3_rec = #l4_to_l3{msgtype = ?L4L3mREQ_TSI_STATUS},
	do_call(ServerRef, {'L4L3m', L4L3_rec, <<>>}).


%% @spec (Channel, LapdId, EnaProtoData) -> true
%% 	Channel = port()
%% 	LapdId = integer()
%% 	EnaProtoData = ena_proto_data()
%%
%% @doc Specifies and enables layer 1, 2 &amp; 3 processing on an open channel.
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

