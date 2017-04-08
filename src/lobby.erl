%% @author: Igor
%% @date: 08.04.2017

-module(lobby).

%% Include files

%% Exported Functions

-export([
    start/0
]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    start_app_recursive(lobby).

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_apps(Apps) ->
    lists:foreach(
        fun(App) ->
            case (catch application:start(App)) of
                ok ->
                    ok;
                {error, {already_started, _}} ->
                    ok;
                Error ->
                    erlang:error({failed_starting, App, Error})
            end
        end, Apps).

start_app_recursive(App) ->
    List = app_deps_recursive(App),
    start_apps(lists:reverse(List)).

app_deps(App) ->
    case application:load(App) of
        ok -> ok;
        {error, {already_loaded, App}} -> ok;
        {error, Error} -> erlang:error(Error)
    end,
    case application:get_key(App, applications) of
        {ok, Deps} -> Deps;
        undefined -> []
    end.

app_deps_recursive(App) ->
    {List, _} = app_deps_recursive(App, sets:new()),
    lists:reverse(List).

app_deps_recursive(App, AllApps) ->
    case sets:is_element(App, AllApps) of
        true -> {[], AllApps};
        false ->
            {List, NewAllApps} = lists:foldl(fun(A, {L, S}) -> 
                {L1, S1} = app_deps_recursive(A, S),
                {L ++ L1, S1}
            end, {[], sets:add_element(App, AllApps)}, app_deps(App)),
            {List ++ [App], NewAllApps}
    end.

