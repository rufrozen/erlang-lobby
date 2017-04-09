%% @author: Igor
%% @date: 08.04.2017

-module(lobby_cowboy).

%% Include files

%% Exported Functions

-export([
    port/0,
    ip/0,
    start_http/0
]).

%%%===================================================================
%%% API
%%%===================================================================

port() ->
    application:get_env(lobby, port, 8080).

ip() ->
    case application:get_env(lobby, server_ip) of
        {ok, Ip} -> [{ip, Ip}];
        _ -> []
    end.

start_http() ->
    BindPort = port(),
    Dispatch = cowboy_router:compile([{'_', agent_handlers()}]),
    Middlewares = [lobby_hook, cowboy_router, cowboy_handler],
    cowboy:start_http(http_agent, 3, [{port, BindPort}] ++ ip(), [{env, [{dispatch, Dispatch}]}, {middlewares, Middlewares}]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

agent_handlers() ->
    [
        {"/api/[...]", lobby_rest_api, undefined},
        {"/websocket", lobby_websocket, undefined}
    ].



