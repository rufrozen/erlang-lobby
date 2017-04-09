%% @author: Igor
%% @date: 09.04.2017

-module(lobby_gproc).

%% Include files

%% Exported Functions

-export([
    init/0,
    gen_ghost_id/0,
    launch/2,
    find/1,
    register/1
]).

%%%===================================================================
%%% API
%%%===================================================================

init() ->
    gproc:add_shared_local_counter(current_ghost_id, 1).

gen_ghost_id() ->
    gproc:update_shared_counter({c,l,current_ghost_id},1).

%% We cannot just make starter return pid cause the new process can fail when registering name
launch(Name, Starter) ->
    case gproc:lookup_local_name(Name) of
        undefined ->
            Ref = gproc:nb_wait({n,l,Name}),
            Starter(),
            receive
                {gproc, Ref, registered, {_,Pid,_}} ->
                    Pid
            after
                5000 ->
                    gproc:cancel_wait({n,l,Name}, Ref),
                    erlang:error({gproc_factory, find_timeout, Name})
            end;
        Pid ->
            Pid
    end.

find(Name) ->
    gproc:lookup_local_name(Name).

register(Name) ->
    try
        gproc:add_local_name(Name), %% Let it crash if the process already exists
        ok
    catch
        _:_ ->  % error badarg
            exit(normal)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================





