-module(lapd_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _Args) ->
	{ok, ServerName} = application:get_env(server_name),
	{ok, DeviceName} = application:get_env(device_name),
	{ok, BoardNumber} = application:get_env(board_number),
	{ok, Timer} = application:get_env(interval),
	{ok, LapdIds} = application:get_env(lapdids),
	StartArgs = [ServerName, DeviceName, BoardNumber, Timer, LapdIds],
	supervisor:start_link(lapd_sup, StartArgs).
	
stop(_State) -> ok.
