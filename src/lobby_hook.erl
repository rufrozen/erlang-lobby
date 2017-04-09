%% @author: Igor
%% @date: 08.04.2017

-module(lobby_hook).

-behaviour(cowboy_middleware).

%% Include files

%% Exported Functions

-export([
    execute/2
]).

%%%===================================================================
%%% API
%%%===================================================================

execute(Req, Env) ->
    Url = cowboy_req:url(Req),
    Method = cowboy_req:method(Req),
    lager:debug([{caption, web}], "~s ~s", [Method, Url]),
    {ok, Req, Env}.

%%%===================================================================
%%% Internal functions
%%%===================================================================





