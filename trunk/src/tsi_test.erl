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
%%% @doc A simple test application which sets up a TSI mapping.
%%%
         
-module(tsi_test).

-include("iisdn.hrl").

-export([map/3]).

%% @spec (Board:port(), Destination::integer(), Source::integer()) -> true
%%
%% @doc Create a TSI mapping.
%%
map(Board, Destination, Source) ->
	Map = #tsi_map{destination = Destination, source =  Source},
	Data = #tsi_data{num_mappings = 1, tsi_map = Map},
	netaccess:set_tsi(Board, Data).

