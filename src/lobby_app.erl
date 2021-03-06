%% @author: Igor
%% @date: 08.04.2017

-module(lobby_app).

-behaviour(application).

%% Application callbacks
-export([
	start/2, 
	stop/1
]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    lobby_gproc:init(),
    lobby_cowboy:start_http(),
    lobby_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


