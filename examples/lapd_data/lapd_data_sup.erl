%%% $Id$

-module(lapd_data_sup).
-author('vances@motivity.ca').
-bsn('$Revsions$').

-behaviour(supervisor).

-export([init/1]).

init([ServerName, DeviceName, BoardNumber, IframeInterval, ReportInterval, LapdIds]) ->
	BoardStartArgs = [ServerName, DeviceName, BoardNumber],
	BoardStartFunc = {netaccess, start_link, BoardStartArgs},
	BoardChildSpec = {netaccess, BoardStartFunc, permanent,
			4000, worker, [netaccess]},
	FsmChildSpecs = init_fsms(ServerName, IframeInterval, ReportInterval, LapdIds, []),
	{ok, {{one_for_one, 10, 60}, [BoardChildSpec] ++ FsmChildSpecs}}.

init_fsms(_ServerName, _IframeInterval, _ReportInterval, [], FsmChildSpecs) ->
	FsmChildSpecs;
init_fsms(ServerName, IframeInterval, ReportInterval, [LapdId|T], FsmChildSpecs) ->
   FsmStartArgs = [lapd_data_fsm, [ServerName, LapdId, symmetric, IframeInterval, ReportInterval], []],
   FsmStartFunc = {netaccess_fsm, start_link, FsmStartArgs},
   FsmChildSpec = {LapdId, FsmStartFunc, permanent,
		4000, worker, [netaccess_fsm, lapd_data_fsm]},
	init_fsms(ServerName, IframeInterval, ReportInterval, T, FsmChildSpecs ++ [FsmChildSpec]).

