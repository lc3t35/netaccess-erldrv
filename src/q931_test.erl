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
%%% @doc A simple test application which starts a D-channel.
%%%
         
-module(q931_test).

-include("iisdn.hrl").

-export([start/2]).

%% @spec (LapdId::integer(), Side) -> void()
%%		Side = user | network | symmetric
%%
%% @doc Start the test.
%%
start(LapdId, Side) ->
	{ok, Channel} = netaccess:open(),
	ok = netaccess:select_board(Channel, 0),
 	L1 = #level1{l1_mode = ?IISDNl1modHDLC},
	case Side of
		user ->
			L2Parms = #l2_lap_params{dce_dte = ?IISDNdirUSER_SIDE},
			Emul = 0;
		network ->
			L2Parms = #l2_lap_params{dce_dte = ?IISDNdirNETWORK_SIDE},
			Emul = 1;
		symmetric ->
			L2Parms = #l2_lap_params{dce_dte = ?IISDNdirSYMMETRIC},
			Emul = 0
	end,
 	L2 = #level2{par = L2Parms},
	Q = #q931{variant = ?IISDNvarNATL_ISDN_2, net_side_emul = Emul},
 	L3 = #level3{l3_mode = ?IISDNl3modQ931, cnfg = Q},
	ProtoData = #ena_proto_data{level1 = L1, level2 = L2, level3 = L3},
	netaccess:enable_protocol(Channel, LapdId, ProtoData),
	loop().

loop() ->
	receive
		Any -> io:fwrite("~p~n", [Any])
	end,
	loop().

