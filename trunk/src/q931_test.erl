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

-export([start/1]).

%% @spec (LapdId::integer()) -> void()
%%
%% @doc Start the test.
%%
start(LapdId) ->
	{ok, Channel} = netaccess:open(),
	ok = netaccess:select_board(Channel, 0),
 	L1 = #level1_cnfg{l1_mode = ?IISDNl1modHDLC},
 	L2Parms = #l2_lap_params{
 			mode = ?IISDNl2modLAP_D, 
 			dce_dte = ?IISDNdirSYMMETRIC},
 	D = #data_interface{enable = 1},
 	L2 = #level2_cnfg{par = L2Parms, data_interface = D},
	Q = #q931_cnfg{variant = ?IISDNvarNATL_ISDN_2},
 	L3 = #level3_cnfg{l3_mode = ?IISDNl3modQ931},
	ProtoData = #ena_proto_data{level1 = L1, level2 = L2, level3 = L3},
	netaccess:enable_protocol(Channel, LapdId, ProtoData),
	loop().

loop() ->
	receive
		Any -> io:fwrite("~p~n", [Any])
	end,
	loop().

