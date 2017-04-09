%% @author: Igor
%% @date: 08.04.2017

-module(lobby_session).

-behaviour(gen_server).

-export([
	start_link/1,
    launch/1,
    find_pid/1,
    auth_login/2,
    auth_stream/2,
    join_channel/4
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    id,
    auth,
    name,
    streams,
    channels,
    timeout
}).

-record(channel, {
    users,
    storage,
    streams_owner,
    streams_guest
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(SessionId) ->
    gen_server:start_link(?MODULE, [SessionId], []).

find_pid({id, SessionId}) ->
    lobby_gproc:find({session, SessionId});
find_pid({auth, Auth}) ->
    lobby_gproc:find({session_auth, Auth});
find_pid(Pid) when is_pid(Pid) ->
    Pid.

launch(SessionId) ->
    lobby_gproc:launch({session, SessionId}, fun() -> proc_lib:spawn(?MODULE, start_link, [SessionId]) end).

auth_login(SessionId, Name) ->
    gen_server:call(launch(SessionId), {auth_login, Name}).

auth_stream(SessionAuth, Pid) ->
    gen_server:call(find_pid({auth, SessionAuth}), {auth_stream, Pid}).

join_channel(SessionId, Channel, GuestSessionId, GuestStreamPid) ->
    gen_server:call(find_pid({id, SessionId}), {join_channel, Channel, GuestSessionId, GuestStreamPid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Id]) ->
    lobby_gproc:register({session, Id}),
    lager:umd(caption, {session, Id}),
    Auth = gen_auth(),
    lobby_gproc:register({session_auth, Auth}),
    State = #state{
        id = Id,
        auth = Auth,
        name = <<"unknown">>,
        streams = [],
        channels = #{},
        timeout = find_timeout(Id)
    },
    {ok, State, State#state.timeout}.

handle_call({auth_login, Name}, _From, State) ->
    #state{auth = Auth} = State,
    Reply = {ok, Auth},
    State1 = State#state{name = Name},
    {reply, Reply, State1, State#state.timeout};
handle_call({auth_stream, Pid}, _From, State) ->
    #state{id = Id, name = Name, streams = Streams} = State,
    erlang:monitor(process, Pid),
    Reply = #{id => Id, name => Name, pid => self()},
    State1 = State#state{name = Name, streams = [Pid|Streams]},
    {reply, Reply, State1, State#state.timeout};
handle_call({join_channel, ChannelKey, GuestSessionId, GuestStreamPid}, _From, State) ->
    #state{id = MyId, channels = Channels} = State,
    Channel = maps:get(ChannelKey, Channels, undefined),
    NewChannel = if
        MyId =:= GuestSessionId, Channel =:= undefined ->
            #channel{storage = #{}, users = [GuestSessionId], streams_guest = [], streams_owner = [GuestStreamPid]};
        MyId =:= GuestSessionId ->
            #channel{streams_owner = StreamsOwner} = Channel,
            Channel#channel{streams_owner = [GuestStreamPid|StreamsOwner]};
        Channel =/= undefined ->
            erlang:monitor(process, GuestStreamPid),
            #channel{users = Users, streams_guest = StreamsQuest} = Channel,
            Channel#channel{users = ordsets:add_element(GuestSessionId, Users), streams_guest = [GuestStreamPid|StreamsQuest]};
        true ->
            undefined
    end,
    if 
        NewChannel =:= undefined -> 
            {reply, undefined, State, State#state.timeout};
        true ->
            State1 = State#state{channels = maps:put(ChannelKey, NewChannel, Channels)},
            Reply = #{pid => self()},
            {reply, Reply, State1, State#state.timeout}
    end;
handle_call(Msg, _From, State) ->
    Reply = ok,
    lager:info("unknown call ~p", [Msg]),
    {reply, Reply, State, State#state.timeout}.

handle_cast({packet, Channel, From, Json}, State) ->
    #state{channels = Channels} = State,
    {noreply, State, State#state.timeout};
handle_cast(Msg, State) ->
    lager:info("unknown cast ~p", [Msg]),
    {noreply, State, State#state.timeout}.

handle_info(timeout, State) ->
    if
        State#state.streams =:= [] ->
            {stop, normal, State};
        true ->
            {noreply, State, State#state.timeout}
    end;
handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    #state{streams = Streams, channels = Channels} = State,
    State1 = State#state{streams = lists:delete(Pid, Streams)},
    {noreply, State1, State#state.timeout};
handle_info(Info, State) ->
    lager:info("unknown info ~p", [Info]),
    {noreply, State, State#state.timeout}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

find_timeout(<<"ghost_", _/binary>>) ->
    application:get_env(lobby, session_timeout, 5*60*1000);
find_timeout(_) ->
    application:get_env(lobby, session_timeout, 60*60*1000).

gen_auth() ->
    lobby_utils:to_hex(crypto:rand_bytes(16)).


