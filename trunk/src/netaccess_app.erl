%%%---------------------------------------------------------------------
%%% @copyright Motivity Telecom Inc. 2001-2004
%%% @author Vance Shipley <vances@motivity.ca>
%%%
%%% All rights reserved. No part of this computer program(s) may be
%%% used, reproduced, stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of
%%% Motivity Telecom Inc.
%%%---------------------------------------------------------------------
%%%
%%% @doc The Netaccess application provides an API and device driver
%%% 		for the Brooktrout Netaccess WAN cards.
%%% @end
%%%
%%% @hidden
         
-module(netaccess_app).
-copyright('Copyright (c) 2001-2004 Motivity Telecom Inc.').
-author('vances@motivity.ca').

-behaviour(application).

%% call backs needed for application behaviour
-export([start/2, prep_stop/1, stop/1, config_change/3]).


%%% @spec start(normal, Args) -> {ok, Pid} | {ok, Pid, State}
%%% 	Pid = pid()
%%% 	State = term()
%%%
%%% @doc Starts the netaccess application.
%%%
start(normal, Listof) ->
	ok;
start({takeover, _Node}, _StartArgs) ->
	{error, not_implemented};
start({failover, _Node}, _StartArgs) ->
	{error, not_implemented}.


%%% @spec (State::term()) -> State
%%%	State = term()
%%%
%%% @doc Called when the application is about to be shut down,
%%% 		before any processes are terminated.
%%%
prep_stop(State) ->
	error_logger:info_report(["Application preparing to stop.",
			{module, ?MODULE}]),
	State.


%%% @spec (State::term()) -> ok
%%%
%%% @doc Called when the application is about to be shut down,
%%% 		before any processes are terminated.
%%%
stop(_State) ->
	ok.


%%% @spec (Changed, New, Removed) -> ok
%%% 	Changed = [{Par, Val}]
%%% 	New = [{Par, Val}]
%%%	Removed = [Par]
%%% 	Par = atom()
%%% 	Val = term()
%%%
%%% @doc Called after a code  replacement, if there are any 
%%% 		changes to the configuration  parameters.
%%%
config_change(_Changed, _New, _Removed) -> ok.

