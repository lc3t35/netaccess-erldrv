-module(lapd_sup).
-behaviour(supervisor).
-export([init/1]).

init([ServerName, DeviceName, BoardNumber, Timer, LapdIds]) ->
	BoardStartArgs = [ServerName, DeviceName, BoardNumber],
	BoardStartFunc = {netaccess, start_link, BoardStartArgs},
	BoardChildSpec = {netaccess, BoardStartFunc, temporary,
			4000, worker, [netaccess]},
	FsmChildSpecs = init_fsms(ServerName, Timer, LapdIds, []),
	{ok, {{one_for_one, 10, 60}, [BoardChildSpec] ++ FsmChildSpecs}}.

init_fsms(ServerName, Timer, [], FsmChildSpecs) ->
	FsmChildSpecs;
init_fsms(ServerName, Timer, [LapdId|T], FsmChildSpecs) ->
   FsmStartArgs = [lapd_fsm, [ServerName, LapdId, Timer], []],
   FsmStartFunc = {netaccess_fsm, start_link, FsmStartArgs},
   FsmChildSpec = {LapdId, FsmStartFunc, temporary,
		4000, worker, [netaccess_fsm, lapd_fsm]},
	init_fsms(ServerName, Timer, T, FsmChildSpecs ++ [FsmChildSpec]).

