-module(lapd_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _Args) ->
	{ok, ServerName} = application:get_env(server_name),
	{ok, DeviceName} = application:get_env(device_name),
	{ok, BoardNumber} = application:get_env(board_number),
	{ok, IframeInterval} = application:get_env(iframe_interval),
	{ok, ReportInterval} = application:get_env(report_interval),
	{ok, LapdIds} = application:get_env(lapdids),
	StartArgs = [ServerName, DeviceName, BoardNumber, IframeInterval,
			ReportInterval, LapdIds],
	supervisor:start_link(lapd_sup, StartArgs).
	
stop(_State) -> ok.
