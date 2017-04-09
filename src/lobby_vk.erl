%% @author: Igor
%% @date: 08.04.2017

-module(lobby_vk).

%% Include files

%% Exported Functions

-export([
    check_sig/1,
    check_sig_login/1,
    check_auth_key/2,
    check_auth_key/4
]).

%%%===================================================================
%%% API
%%%===================================================================

check_sig_login(Session) ->
    Session1 = maps:with([<<"expire">>, <<"mid">>, <<"secret">>, <<"sid">>, <<"sig">>], Session),
    check_sig(Session1).

check_sig(Query) ->   
    Sig = maps:get(<<"sig">>, Query),
    Request = lists:keysort(1, maps:to_list(maps:remove(<<"sig">>, Query))),
    MySigBinary = iolist_to_binary([[[K, "=", lobby_utils:to_binary(V)] || {K, V} <- Request], secret_key()]),
    lobby_utils:md5(MySigBinary) =:= Sig.

check_auth_key(AuthHash, ViewerId) ->
    check_auth_key(AuthHash, app_id(), ViewerId, secret_key()).

check_auth_key(AuthHash, ApiId, ViewerId, SecretKey) ->
    Binary = list_to_binary([lobby_utils:to_binary(ApiId), lobby_utils:to_binary(ViewerId), lobby_utils:to_binary(SecretKey)]),
    lobby_utils:md5(Binary) =:= lobby_utils:to_binary(AuthHash).

%%%===================================================================
%%% Internal functions
%%%===================================================================

params() ->
    {ok, List} = application:get_env(lobby, vk),
    List.

app_id() ->
    proplists:get_value(app_id, params(), {empty_app_id}).

secret_key() ->
    proplists:get_value(secret_key, params(), {empty_secret_key}).