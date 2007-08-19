%%% $Id$
%%%---------------------------------------------------------------------
%%% @copyright (c) 2001-2007 Vance Shipley
%%% @author Vance Shipley <vances@motivity.ca>
%%% @end
%%% 
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 
%%% Redistributions of source code must retain the above copyright 
%%% notice, this list of conditions and the following disclaimer.
%%% 
%%%     - Redistributions in binary form must reproduce the above 
%%%       copyright notice, this list of conditions and the following 
%%%       disclaimer in the documentation and/or other materials 
%%%       provided with the distribution.
%%%     - Neither the name of Motivity Telecom nor the names of its 
%%%       contributors may be used to endorse or promote products 
%%%       derived from this software without specific prior written 
%%%       permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
%%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
%%% COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN 
%%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
%%% POSSIBILITY OF SUCH DAMAGE.
%%% 
%%%---------------------------------------------------------------------
%%%
%%% @doc Main API of the netaccess application.
%%%	<p>This module provides the main application programming interface
%%%	for the netaccess application.  The application includes a
%%%	dynamically linked device driver to the Netaccess&#153; series boards
%%%	from Brooktrout Technology.  This module is an Erlang binding to
%%%	the Instant ISDN&#153; Simple Message Interface (SMI) API.</p>
%%%
%%% <a name="type-ena_proto_data" href="iisdn.html#type-ena_proto_data"></a>
%%% @end
%%%
%%% @reference <a href="http://www.brooktrout.com/products/ns301/">
%%% 		NS301 product information</a>
%%% @reference <a href="iisdn.html">iisdn module</a>
%%% @reference <a href="pridrv.html">pridrvmodule</a>
%%%
         
-module(netaccess).
-copyright('Copyright (c) 2001-2007 Vance Shipley').
-author('vances@motivity.ca').
-vsn('$Revision$').

%% our published API functions
-export([start/0, start/2, start/3]).
-export([start_link/2, start_link/3]).
-export([stop/1]).
-export([open/1, close/1]).
-export([reset_board/1, boot/2]).
-export([get_version/1, get_driver_info/1]).
-export([set_hardware/2, req_hw_status/1]).
-export([req_board_id/1]).
-export([set_tsi/2, req_tsi_status/1]).
-export([get_qsize/1, set_maxiframesize/2, set_lowwater/2, set_highwater/2]).
-export([enable_protocol/3, disable_protocol/3, req_protocol_status/2]).
-export([relay_add_rule/4, relay_clear_rules/3, relay_del_rule/4]).
-export([req_l2_stats/2, pm_request/3]).
-export([send/2]).

-include("pridrv.hrl").
-include("iisdn.hrl").

-define(REQ_TIMEOUT, 15000).  % 15sec command timeout

%%----------------------------------------------------------------------
%%  The API functions
%%----------------------------------------------------------------------

%% @spec () -> Server
%%		Server = pid()
%%
%% @doc Start the netacccess server.
%% 	<p>Creates a <tt>netaccess_server</tt> process to handle requests
%% 	for the board.  The server opens a management channel to the board 
%% 	which is used to handle board level requests and indications.</p>
%%
%% 	<p>If the <tt>netaccess_server</tt> is successfully created and 
%% 	initialized the function returns <tt>{ok, Server}</tt> where 
%% 	<tt>Server</tt> is the <tt>pid()</tt> of the 
%% 	<tt>netaccess_server</tt>.  Otherwise it returns 
%% 	<tt>{error, Reason}</tt>.</p>
%%
%% 	<p><b>Note:</b>  This is a convenience function which uses the default board
%% 	name <tt>"/dev/pri0"</tt> and board number <tt>0</tt>.</p>
%%
%% @end
%%
start() ->
	case gen_server:start(netaccess_server, [?DEFAULT_BOARDNAME, 0], []) of
		{ok, Server} ->
			Server;
		{error, Reason} ->
			exit(Reason)
	end.
	
%% @spec (BoardName, BoardNumber) -> Server
%% 	BoardName = string()
%% 	BoardNumber = integer()
%%		Server = pid()
%%
%% @doc Start the netacccess server on a specific board.
%% 	<p>Creates a <tt>netaccess_server</tt> process to handle 
%% 	requests for the board with device name <tt>BoardName</tt>.
%% 	The board number is needed to to tie the opened <tt>STREAMS</tt>
%% 	clone device to the correct device instance.</p>
%%
%% 	<p>e.g. <code>netaccess:start("/dev/pri0", 0)</code></p>
%%
%% @end
%%
%%
start(BoardName, BoardNumber) ->
	case gen_server:start(netaccess_server, [BoardName, BoardNumber], []) of
		{ok, Server} ->
			Server;
		{error, Reason} ->
			exit(Reason)
	end.
	
%% @spec (ServerName, BoardName, BoardNumber) -> Server
%% 	ServerName = {local,Name} | {global,Name}
%% 	BoardName = string()
%% 	BoardNumber = integer()
%% 	Name = atom()
%% 	Node = atom()
%%		Server = pid()
%%
%% @doc Start the netacccess server using a registered name.
%% 	<p>If <tt>ServerName</tt> = <tt>{local,Name}</tt> the process
%% 	is registered locally using <tt>register/2</tt>.  If
%% 	<tt>ServerName</tt> = <tt>{global,Name}</tt> the process
%% 	is registered globally using <tt>global:register_name/2</tt>.</p>
%%
%% @end
%%
start(ServerName, BoardName, BoardNumber) ->
	case gen_server:start(ServerName, netaccess_server, [BoardName, BoardNumber], []) of
		{ok, Server} ->
			Server;
		{error, Reason} ->
			exit(Reason)
	end.
	
%% @spec (BoardName, BoardNumber) -> {ok, Server} | {error, Reason}
%% 	BoardName = string()
%% 	BoardNumber = integer()
%%		Server = pid()
%%		Reason = term()
%%
%% @doc Start the netacccess server and link to the calling process.
%%
%% 	<p>This function is suitable for use as a start function in a 
%% 	supervisor child specification.</p>
%%
%% @see start/2
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
%% 	<p>This function is suitable for use as a start function in a 
%% 	supervisor child specification.</p>
%%
%% @see start/3
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
	gen_server:cast(ServerRef, stop).


%% @spec (ServerRef) -> Channel
%% 	ServerRef = Name | {Name, Node} | pid()
%% 	Name = atom()
%% 	Node = atom()
%%		Channel = port()
%%
%% @doc Open a channel on a netaccess board.
%% 	<p>Opens a channel on the board for exclusive use by the caller.</p>
%%
%% 	<p>Returns a <tt>port()</tt> to the driver which has been linked
%% 	to the calling processes using <tt>port_connect/2</tt>.  Fails 
%% 	otherwise.</p>
%%
%% 	<p><b>Note:</b>  It is not possible to open a channel on a remote node.</p>
%%
%% @end
%%
%%
open(ServerRef) ->
	gen_server:call(ServerRef, open).

%% @spec (Channel) -> true
%% 	Channel = port()
%%
%% @doc Close an open channel on a netaccess board.
%% 	<p>Closes the <tt>port()</tt> to the driver.</p>
%%
%%
close(Port) ->
	port_close(Port).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%% IOCTL calls                                                           %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec (ServerRef, BootFile) -> ok
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%%		BootFile = binary() | string() | atom()
%%		Reason = term()
%%
%% @doc Boot an open netaccess board.
%%		<p>BootFile may be either the boot code itself in a <tt>binary()</tt>
%%		or the file name of the boot file.</p>
%%
%%		<p><b>Note:</b>If the kernel is 64 bit the Erlang emulator, as well
%%		as the netaccess driver, must be compiled as 64 bit for
%%		this to work.  Otherwise fails with <tt>badarg</tt>.</p>
%%
%% @end
%%
boot(ServerRef, BootBin) when binary(BootBin) ->
	gen_server:call(ServerRef, {ioctl, ?BOOT_BOARD, BootBin}, 62000);
boot(ServerRef, Filename) when list(Filename) ->
	case catch file:read_file(Filename) of
		{ok, BootBin} ->
			boot(ServerRef, BootBin);
		Error ->
			exit(Error)	
	end.


%% @spec (ServerRef) -> ok
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%%
%% @doc Perform a reset on an open netaccess board.
%% 	<p>Resets the board and runs internal diagnostics.
%% 	This may take ten seconds or more to complete.  Afterwards
%% 	the board will need to be downloaded again.</p>
%%
%% @end
%%
reset_board(ServerRef) ->
	gen_server:call(ServerRef, {ioctl, ?RESET_BOARD, 0}).


%% @spec (ServerRef) -> Version
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%%		Version = string()
%%
%% @doc Retrieve software version string from an open netaccess board.
%% 	
get_version(ServerRef) ->
	gen_server:call(ServerRef, {ioctl, ?GET_VERSION, 0}).


%% @spec (ServerRef) -> DriverInfo
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%% 	DriverInfo = pridrv:driver_info()
%%
%% @see pridrv:driver_info()
%%
%% @doc Retrieve driver information from an open netaccess board.
%%
get_driver_info(ServerRef) ->
	DriverInfo = gen_server:call(ServerRef, {ioctl, ?GET_DRIVER_INFO, 0}),
	pridrv:driver_info(DriverInfo).
	

%% @spec (Channel) -> QSize
%% 	Channel = port()
%% 	QSize = integer()
%%
%% @doc Retrieve the current size of the outbound queue.
%% 	<p>Messages to the board are handled in their own threads
%% 	in the driver.  If the stream is flow controlled these 
%% 	requests may not complete immediately.  This function will
%% 	return the number of pending requests still running.</p>
%%
%% @end
%%
get_qsize(Channel) ->
	erlang:port_call(Channel, ?QSIZE, 0).

%% @spec (Channel, NewValue) -> OldValue
%% 	Channel = port()
%% 	NewValue = integer()
%% 	OldValue = integer()
%%
%% @doc Set the highwater mark for the outbound queue.
%% 	<p>A highwater mark is maintained in the queue.  Once
%% 	this mark is reached the port is set as busy causing
%% 	the calling process to block.  Once the queue drains
%% 	to the lowwater mark the port will be set to not busy.</p>
%%
%% 	<p>Returns the previous value of the highwater mark.</p>
%%
set_highwater(Channel, NewValue) ->
	erlang:port_call(Channel, ?HIGHWATER, NewValue).

%% @spec (Channel, NewValue) -> OldValue
%% 	Channel = port()
%% 	NewValue = integer()
%% 	OldValue = integer()
%%
%% @doc Set the lowwater mark for the outbound queue.
%% 	<p>The lowwater mark determines when to remove flow
%% 	control by setting the port as not busy after it has
%% 	previously been set busy due to the highwater mark
%% 	having been reached.</p>
%% 	<p>Returns the previous value of the lowwater mark.</p>
%%
set_lowwater(Channel, NewValue) ->
	erlang:port_call(Channel, ?LOWWATER, NewValue).

%% @spec (Channel, NewValue) -> OldValue
%% 	Channel = port()
%% 	NewValue = integer()
%% 	OldValue = integer()
%%
%% @doc Set the maximum size of a received IFRAME.
%% 	<p>A <tt>binary()</tt> must be allocated to receive an incoming
%% 	data message into whenever an event is signaled from
%% 	an open channel on the board.  To minimize the memory
%% 	requirements of the system a small <tt>binary()</tt>, suitable
%% 	for typical telephony signaling messages, is allocated.</p>
%%
%% 	<p>This function may be used to allocate a larger, or smaller
%% 	<tt>binary()</tt> to receive incoming IFRAMEs.  Incoming IFRAMEs larger
%% 	than this value will be discarded.</p>
%%
%% 	<p><b>Note:</b>  The default is <tt>260</tt> bytes as recommended in
%% 	ITU-T Recommendation Q.921.</p>
%%
set_maxiframesize(Channel, NewValue) ->
	erlang:port_call(Channel, ?MAXIFRAMESIZE, NewValue).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%% SMI messages                                                          %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec (ServerRef) -> BoardId
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%% 	BoardId = iisdn:board_id()
%%
%% @doc Retrieve board identification.
%%
req_board_id(ServerRef) ->
	L4L3_rec = #l4_to_l3{msgtype = ?L4L3mREQ_BOARD_ID},
	gen_server:call(ServerRef, {'L4L3m', L4L3_rec, <<>>}).


%% @spec (ServerRef, HardWareData) -> ok
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%% 	HardwareData = iisdn:hardware_data()
%%
%% @doc Set hardware settings on a board.
%%
set_hardware(ServerRef, HwData) when is_record(HwData, hardware_data) ->
	DataBin = iisdn:hardware_data(HwData),
	L4L3_rec = #l4_to_l3{msgtype = ?L4L3mSET_HARDWARE, data = DataBin},
	gen_server:call(ServerRef, {'L4L3m', L4L3_rec, <<>>}).


%% @spec (ServerRef) -> HardwareData
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%% 	HardwareData = iisdn:hardware_data()
%%
%% @doc Query the hardware setup.
%%
req_hw_status(ServerRef) ->
	L4L3_rec = #l4_to_l3{msgtype = ?L4L3mREQ_HW_STATUS},
	gen_server:call(ServerRef, {'L4L3m', L4L3_rec, <<>>}).
	
	
%% @spec (ServerRef, TsiData) -> ok
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%% 	TsiData = iisdn:tsi_data()
%%
%% @doc Create timeslot mappings.
%%
set_tsi(ServerRef, TsiData) when is_record(TsiData, tsi_data) ->
	DataBin = iisdn:tsi_data(TsiData),
	L4L3_rec = #l4_to_l3{msgtype = ?L4L3mSET_TSI, data = DataBin},
	gen_server:call(ServerRef, {'L4L3m', L4L3_rec, <<>>}).


%% @spec (ServerRef) -> TsiDataList
%% 	ServerRef = Name | {Name, Node} | {global, Name} | pid()
%% 	Name = atom()
%% 	Node = atom()
%% 	TsiDataList = [iisdn:tsi_data()]
%%
%% @doc Retrieve the timeslot mappings.
%%
req_tsi_status(ServerRef) -> 
	L4L3_rec = #l4_to_l3{msgtype = ?L4L3mREQ_TSI_STATUS},
	gen_server:call(ServerRef, {'L4L3m', L4L3_rec, <<>>}).


%% @spec (Channel, LapdId, EnaProtoData) -> Status
%% 	Channel = port()
%% 	LapdId = integer()
%% 	EnaProtoData = iisdn:ena_proto_data()
%% 	Status = iisdn:protocol_stat()
%%
%% @doc Specifies and enables layer 1, 2 &amp; 3 processing on an open channel.
%%
enable_protocol(Channel, LapdId, EnaProtoData)  ->
	enable_protocol(Channel, LapdId,  0, EnaProtoData).

%% @spec (Channel, LapdId, LogicalLinkID, EnaProtoData) -> Status
%% 	Channel = port()
%% 	LapdId = integer()
%% 	LogicalLinkID = integer()
%% 	EnaProtoData = iisdn:ena_proto_data()
%% 	Status = iisdn:protocol_stat()
%%
%% @doc Specifies and enables layer 1, 2 &amp; 3 processing on an open channel.
%%
enable_protocol(Channel, LapdId, LogicalLinkID, #ena_proto_data{} = EnaProtoData) ->
	Data = iisdn:ena_proto_data(EnaProtoData),
	L4L3 = #l4_to_l3{lapdid = LapdId, lli = LogicalLinkID, msgtype = ?L4L3mENABLE_PROTOCOL, data = Data},
	L3L4 = call_l4l3m(Channel, L4L3, ?L3L4mPROTOCOL_STATUS),
	iisdn:protocol_stat(L3L4#l3_to_l4.data).

%% @spec (Channel, LapdId, LogicalLinkID, RelayRules) -> ok
%% 	Channel = port()
%% 	LapdId = integer()
%% 	LogicalLinkID = integer()
%% 	RelayRules = [iisdn:relay_rule()]
%%
%% @doc Add relay rules on a channel previously enabled for packet relay.
%%
relay_add_rule(Channel, LapdId, LogicalLinkID, RelayRules) when length(RelayRules) =< 8 ->
	relay_add_rule(Channel, LapdId, LogicalLinkID, RelayRules, <<>>).
%% @hidden
relay_add_rule(Channel, LapdId, LogicalLinkID, [RelayRule|T], Rules) ->
	NewRule = iisdn:relay_rule(RelayRule),
	NewBin = <<Rules/binary, NewRule/binary>>,
	relay_add_rule(Channel, LapdId, LogicalLinkID, T, NewBin);
relay_add_rule(Channel, LapdId, LogicalLinkID, [], Rules) ->
	EmptyRule = iisdn:relay_rule(#relay_rule{}),
	Data = <<Rules/binary, EmptyRule/binary>>,
	L4L3 = #l4_to_l3{lapdid = LapdId, lli = LogicalLinkID, msgtype = ?L4L3mRELAY_ADD_RULE, data = Data},
	cast_l3l4m(Channel, L4L3).
	

%% @spec (Channel, LapdId, LogicalLinkID) -> ok
%% 	Channel = port()
%% 	LapdId = integer()
%% 	LogicalLinkID = integer()
%%
%% @doc Clear all relay rules on a channel previously enabled for packet relay.
%%
relay_clear_rules(Channel, LapdId, LogicalLinkID) ->
	L4L3 = #l4_to_l3{lapdid = LapdId, lli = LogicalLinkID, msgtype = ?L4L3mRELAY_CLEAR_RULES},
	cast_l3l4m(Channel, L4L3).
	

%% @spec (Channel, LapdId, LogicalLinkID, RelayRule) -> ok
%% 	Channel = port()
%% 	LapdId = integer()
%% 	LogicalLinkID = integer()
%% 	RelayRule = iisdn:relay_rule()
%%
%% @doc Delete a relay rule previously added on a channel enabled for packet relay.
%%
relay_del_rule(Channel, LapdId, LogicalLinkID, #relay_rule{} = RelayRule) ->
	Data = iisdn:relay_rule(RelayRule),
	L4L3 = #l4_to_l3{lapdid = LapdId, lli = LogicalLinkID, msgtype = ?L4L3mRELAY_DEL_RULE, data = Data},
	cast_l3l4m(Channel, L4L3).
	

%% @spec (Channel, LapdId, LogicalLinkID) -> ok
%% 	Channel = port()
%% 	LapdId = integer()
%% 	LogicalLinkID = integer()
%% 	EnaProtoData = iisdn:ena_proto_data()
%%
%% @doc Disable the protocol stack running on an HDLC channel.
%%
disable_protocol(Channel, LapdId, LogicalLinkID) ->
	L4L3 = #l4_to_l3{lapdid = LapdId, lli = LogicalLinkID, msgtype = ?L4L3mDISABLE_PROTOCOL},
	cast_l3l4m(Channel, L4L3).
	

%% @spec (Channel, LapdId) -> ProtocolStatus
%% 	Channel = port()
%% 	LapdId = integer()
%% 	ProtocolStatus = iisdn:protocol_stat()
%%
%% @doc Retrieves the current protocol status for an active channel.
%% 	<p>NOTE:  not implemented yet in IISDN v7.6</p>
%%
req_protocol_status(Channel, LapdId) ->
	L4L3 = #l4_to_l3{lapdid = LapdId, msgtype = ?L4L3mREQ_PROTOCOL_STATUS},
	L3L4 = call_l4l3m(Channel, L4L3, ?L3L4mPROTOCOL_STATUS),
	iisdn:protocol_stat(L3L4#l3_to_l4.data).

%% @spec (Channel, LapdId) -> L2Stats
%% 	Channel = port()
%% 	LapdId = integer()
%% 	L2Stats = iisdn:l2_stats() | iisdn:mtp2_stats()
%%
%% @doc Retrieves level 2 statistics for a channel.
%%
req_l2_stats(Channel, LapdId) ->
	L4L3 = #l4_to_l3{lapdid = LapdId, msgtype = ?L4L3mREQ_L2_STATS},
	L3L4 = call_l4l3m(Channel, L4L3, ?L3L4mL2_STATS),
	iisdn:pm_rsp_data(L3L4#l3_to_l4.data).
	
%% @spec (Channel, LapdId, Request) -> Response
%% 	Channel = port()
%% 	LapdId = integer()
%% 	Request = iisdn:pm_req_data()
%% 	Response = ok | iisdn:pm_rsp_data()
%%
%% @doc Control performance monitoring.
%% 	<p>Some performance monitoring commands generate response data.
%% 	Others immediately return <tt>ok</tt> (i.e. <tt>SET</tt>).</p>
%%
pm_request(Channel, LapdId, #pm_req_data{pm_cmd = Cmd} = Request) when
		% asynchronous commands
		Cmd == ?IISDNpmcSET_THRSHLDS_15MIN;
		Cmd == ?IISDNpmcSET_THRSHLDS_24HR;
		Cmd == ?IISDNpmcRESET_PM_COUNTERS;
		Cmd == ?IISDNpmcPLB_ACTIVATE;
		Cmd == ?IISDNpmcPLB_DEACTIVATE;
		Cmd == ?IISDNpmcLLB_ACTIVATE;
		Cmd == ?IISDNpmcLLB_DEACTIVATE;
		Cmd == ?IISDNpmcSEND_FDL_REQUEST ->
	pm_request(Channel, LapdId, Request, false);
pm_request(Channel, LapdId, #pm_req_data{pm_cmd = Cmd} = Request) when
		% response commands
		Cmd == ?IISDNpmcGET_THRSHLDS_15MIN;
		Cmd == ?IISDNpmcGET_THRSHLDS_24HR;
		Cmd == ?IISDNpmcGET_15MIN_DATA;
		Cmd == ?IISDNpmcGET_24HR_DATA ->
	pm_request(Channel, LapdId, Request, true).
pm_request(Channel, LapdId, #pm_req_data{} = Request, Synch) ->
	Data = iisdn:pm_req_data(Request),
	L4L3 = #l4_to_l3{lapdid = LapdId, msgtype = ?L4L3mPM_REQUEST, data = Data},
	pm_request1(Channel, L4L3, Synch).
%% @hidden
pm_request1(Channel, L4L3, true) ->
	L3L4 = call_l4l3m(Channel, L4L3, ?L3L4mPM_RESPONSE),
	iisdn:pm_rsp_data(L3L4#l3_to_l4.data);
pm_request1(Channel, L4L3, false) ->
	cast_l3l4m(Channel, L4L3).

%% @spec (Channel, Iframe) -> true
%% 	Channel = port()
%% 	Iframe= binary() | iolist()
%%
%% @doc Sends a data message on an open channel.
%%
send(Channel, Iframe) ->
	port_command(Channel, Iframe).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%% internal functions                                                    %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec(Channel, L4L3, Response) -> Result
%% 	Channel = port()
%% 	L4L3 = iisdn:l4_to_l3()
%% 	Response = integer()
%% 	Result = iisdn:l3_to_l4()
%%
%% @doc Send an synchronous L4L3m and wait for the matching response.
%%
%% @hidden
%%
call_l4l3m(Channel, L4L3, Response) ->
	erlang:port_call(Channel, ?L4L3m, iisdn:l4_to_l3(L4L3)),
	LapdId = L4L3#l4_to_l3.lapdid,
	Request = L4L3#l4_to_l3.msgtype,
	receive
		% the expected L3L4m response
		{Channel, {'L3L4m', <<LapdId:?IISDNu8bit, Response:?IISDNu8bit, _/binary>> = L3L4, _}} ->
  			iisdn:l3_to_l4(L3L4);
		% an L3L4mERROR message with offending_message matching request
		{Channel, {'L3L4m', <<LapdId:?IISDNu8bit, ?L3L4mERROR:?IISDNu8bit, 
				_L4_ref:?IISDNu16bit, _Call_ref:?IISDNu16bit, _Bchanel:?IISDNu8bit, _Iface:?IISDNu8bit,
				_Bchannel_mask:?IISDNu32bit, _Lli:?IISDNu16bit, _Data_channel:?IISDNu16bit,
				ErrorCode:?IISDNu8bit, Request:?IISDNu8bit, Extra/binary>>, _}} ->
			ErrorRec = iisdn:error_code(<<ErrorCode:?IISDNu8bit, Request:?IISDNu8bit, Extra/binary>>),
			exit(ErrorRec#error_code.error_code)
	after ?REQ_TIMEOUT ->
		exit(timeout)
	end.

%% @spec(Channel, L4L3) -> ok
%% 	Channel = port()
%% 	L4L3 = iisdn:l4_to_l3()
%%
%% @doc Send an asynchronous L4L3m.
%%
%% @hidden
%%
cast_l3l4m(Channel, L4L3) ->
	erlang:port_call(Channel, ?L4L3m, iisdn:l4_to_l3(L4L3)).

