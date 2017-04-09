%% @author: Igor
%% @date: 08.04.2017

-module(lobby_websocket).

-behaviour(cowboy_websocket).

%% Include files

%% Exported Functions

-export([
    init/2,
    websocket_handle/3,
    websocket_info/3
]).

-record(state, {
    service_pid,
    channel,
    chunks
}).

-define(new_line, 10).

%%%===================================================================
%%% API
%%%===================================================================

init(Req, Opt) ->
    lager:umd(caption, websocket),
    Query = maps:from_list(cowboy_req:parse_qs(Req)),
    case Query of
        #{<<"auth">> := Auth, <<"owner">> := OwnerId, <<"channel">> := Channel} ->
            #{id := SessionId, pid := SessionPid} = lobby_session:auth_stream(Auth, self()),
            #{pid := OwnerPid} = lobby_session:join_channel(OwnerId, Channel, SessionId, self()),
            erlang:monitor(process, OwnerPid),
            if OwnerPid =/= SessionPid -> erlang:monitor(process, SessionPid);
                true -> ignore
            end,
            State = #state{chunks = [], channel = Channel, service_pid = SessionPid},
            {cowboy_websocket, Req, State};
        _ ->
            {ok, Req, Opt}
    end.

websocket_handle({text, Msg}, Req, State) ->
    State1 = recv(State, Msg),
	{ok, Req, State1};
websocket_handle({binary, Msg}, Req, State) ->
    lager:info("unknown binary packet: ~p", [size(Msg)]),
	{ok, Req, State};
websocket_handle({ping, _Msg}, Req, State) ->
	{reply, pong, Req, State};
websocket_handle(Data, Req, State) ->
    lager:info("unknown event: ~p", [Data]),
	{ok, Req, State}.

websocket_info({'$gen_cast', {send, Packets}}, Req, State) ->
    Reply = pack(Packets),
	{reply, {text, Reply}, Req, State};
websocket_info({send, Packets}, Req, State) ->
    Reply = pack(Packets),
	{reply, {text, Reply}, Req, State};
websocket_info({'$gen_cast', disconnect}, Req, State) ->
	{stop, Req, State};
websocket_info(disconnect, Req, State) ->
	{stop, Req, State};
websocket_info({'DOWN', _MonitorRef, process, _Pid, _Reason}, Req, State) ->
	{stop, Req, State};
websocket_info(Info, Req, State) ->
    lager:info("unknown info: ~p", [Info]),
	{ok, Req, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

recv(<<>>, State) ->
    State;
recv(Body, State) ->
    #state{chunks = Chunks} = State,
    case binary:split(Body, <<?new_line>>) of
        [Head, Tail] ->
            State1 = State#state{chunks = []},
            Packet = iolist_to_binary(lists:reverse([Head|Chunks])),
            State2 = chunk_completed(Packet, State1),
            recv(Tail, State2);
        [Val] ->
            State#state{chunks = [Val|Chunks]}
    end.

chunk_completed(<<>>, State) ->
    State;
chunk_completed(Packet, State) ->
    #state{service_pid = ServicePid, channel = Channel} = State,
    Json = jsx:decode(Packet, [return_maps]),
    gen_server:cast(ServicePid, {packet, Channel, self(), Json}),
    State.

pack(Packets) ->
    iolist_to_binary([[jsx:encode(Packet), <<"\n">>] || Packet <- Packets]).


