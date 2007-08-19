%%% $Id$
%%%---------------------------------------------------------------------
%%% @copyright 2004-2007 Vance Shipley
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
%%% @doc A simple test application which starts a D-channel.
%%%
         
-module(q931_test).
-copyright('Copyright (c) 2001-2007 Vance Shipley').
-author('vances@motivity.ca').
-vsn('$Revision$').

-include("iisdn.hrl").

-export([start/2]).

%% @spec (LapdId::integer(), Side) -> void()
%%		Side = user | network | symmetric
%%
%% @doc Start the test.
%%
start(LapdId, Side) ->
	Channel = netaccess:open(),
	netaccess:select_board(Channel, 0),
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

