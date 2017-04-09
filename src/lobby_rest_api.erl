%% @author: Igor
%% @date: 08.04.2017

-module(lobby_rest_api).

-behaviour(cowboy_rest).

%% Include files

%% Exported Functions

-export([
    init/2,
    forbidden/2,
    is_conflict/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    charsets_provided/2,
    delete_resource/2,
    handle_json/2
]).

%%%===================================================================
%%% API
%%%===================================================================

init(Req, State) ->
	{cowboy_rest, Req, State}.

forbidden(Req, State) ->
    {false, Req, State}.

is_conflict(Req, State) ->
    {false, Req, State}.

allowed_methods(Req, State) ->
    HttpMethods = [<<"GET">>],
    {HttpMethods, Req, State}.

content_types_accepted(Req, State) ->
    ContentTypes = [
        {{<<"application">>, <<"json">>, '*'}, handle_json}
    ],
    {ContentTypes, Req, State}.

content_types_provided(Req, State) ->
    ContentTypes = [
        {{<<"application">>, <<"json">>, '*'}, handle_json}
    ],
    {ContentTypes, Req, State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

delete_resource(Req, State) ->
    handle_json(Req, State).

handle_json(Req, State) ->
    try
         callback(Req, State)
    catch
        bad_request ->
            Req1 = cowboy_req:reply(400, [], <<"400 Bad Request">>, Req),
            {stop, Req1, State};
        forbidden ->
            Req1 = cowboy_req:reply(403, [], <<"403 Forbidden">>, Req),
            {stop, Req1, State};
        not_found -> 
            Req1 = cowboy_req:reply(404, [], <<"404 Not Found">>, Req),
            {stop, Req1, State};
        internal_server_error ->
            Req1 = cowboy_req:reply(500, [], <<"500 Internal Server Error">>, Req),
            {stop, Req1, State};
        not_implemented ->
            Req1 = cowboy_req:reply(501 , [], <<"501 Not Implemented">>, Req),
            {stop, Req1, State};
        _Error:Reason ->
            lager:error(lager:format_exception(Reason)),
            Req1 = cowboy_req:reply(500, [], <<"500 Internal Server Error">>, Req),
            {stop, Req1, State}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

callback(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            Query = maps:from_list(cowboy_req:parse_qs(Req)),
            Json = on_get(cowboy_req:path_info(Req), Query),
            Binary = jsx:encode(Json),
            {Binary, Req, State};
        <<"POST">> ->
            Query = maps:from_list(cowboy_req:parse_qs(Req)),
            {ok, Body, Req2} = cowboy_req:body(Req),
            BodyJson = jsx:decode(Body, [return_maps]),
            Json = on_post(cowboy_req:path_info(Req2), Query, BodyJson),
            Binary = jsx:encode(Json),
            Req3 = cowboy_req:set_resp_body(Binary, Req2),
            {true, Req3, State};
        _ ->
            throw(not_found)
    end.

on_get(_Path, _Query) ->
    throw(not_found).

on_post([<<"auth">>, <<"ghost">>], _Query, Json) ->
    #{
        <<"name">> := Name
    } = Json,
    Rand = integer_to_binary(lobby_gproc:gen_ghost_id()),
    Id = <<"ghost_", Rand/binary>>,
    {ok, Auth} = lobby_session:auth_login(Id, Name),
    #{auth => Auth, id => Id, name => Name};
on_post([<<"auth">>, <<"vk">>], _Query, Json) ->
    case lobby_vk:check_sig_login(Json) of
        true ->
            #{
                <<"id">> := VkId,
                <<"first_name">> := FirstName,
                <<"last_name">> := LastName
            } = maps:get(<<"user">>, Json),
            Id = <<"vk_", VkId/binary>>,
            Name = <<FirstName/binary, " ", LastName/binary>>,
            {ok, Auth} = lobby_session:auth_login(Id, Name),
            #{auth => Auth, id => Id, name => Name};
        _ -> 
            throw(forbidden)
    end;
on_post(_Path, _Query, _Json) ->
    throw(not_found).
