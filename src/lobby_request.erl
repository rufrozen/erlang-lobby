%% @author: Igor
%% @date: 08.04.2017

-module(lobby_request).

%% Include files

%% Exported Functions

-export([
    get/3,
    request/5
]).

-define(TIMEOUT, 10000). % 10 seconds

%%%===================================================================
%%% API
%%%===================================================================

get(ApiUrl, Path, Args) ->
    request(ApiUrl, get, Path, Args, []).

request(ApiUrl, Method, Path, Args, Body) ->
    Url = make_url(ApiUrl, Path, Args),
    Headers = [{"Host", ApiUrl}],
    Options = [{sync, true}, {headers_as_is, true}, {connect_timeout, ?TIMEOUT}, {response_format, binary}], 
    case ibrowse:send_req(Url, Headers, Method, Body, Options, ?TIMEOUT) of
        {ok, "200", _ResponseHeaders, ResponseBody} ->
            parse_body(ResponseBody);
        {ok, Status, _ResponseHeaders, ResponseBody} ->
            lager:error("request: ~p ~p~n body:~p~n", [wrong_status, Status, ResponseBody]),
            {error, wrong_status};
        {ibrowse_req_id, _ReqId} ->
            lager:error("request error: ~p~n", [wrong_response]),
            {error, wrong_response};
        {error, Reason} ->
            lager:error("request error: ~p~n", [Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

to_list(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_list(Binary) when is_binary(Binary) -> 
    binary_to_list(Binary);
to_list(List) when is_list(List) -> 
    List.

make_url(Host, Path, Params) ->
    lists:flatten([
        "https://", Host, "/",
        join([to_list(T) || T <- Path], "/"),
        ["?" || Params =/= []],
        make_params(Params)
    ]).

make_params(Params) ->
    join([[ibrowse_lib:url_encode(K), "=", ibrowse_lib:url_encode(to_list(V))] || {K, V} <- Params], "&").

join([], _Separator) ->
    [];
join([H|Tail], Separator) ->
    [H, [ [Separator, I] || I <- Tail]].

parse_body(Body) ->
    try 
        Response = jsx:decode(Body, [return_maps]),
        {json, Response}
    catch
        _:_ -> 
            {binary, Body}
    end.

