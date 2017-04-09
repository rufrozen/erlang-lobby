%% @author: Igor
%% @date: 08.04.2017

-module(lobby_utils).

%% Include files

%% Exported Functions

-export([
    md5/1,
    to_binary/1,
    to_hex/1
]).

%%%===================================================================
%%% API
%%%===================================================================

md5(Binary) ->
    to_hex(erlang:md5(Binary)).

to_hex(Binary) ->
    list_to_binary([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Binary)]).

to_binary(Binary) when is_binary(Binary) -> Binary;
to_binary(List) when is_list(List) -> list_to_binary(List);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, latin1);
to_binary(Integer) when is_integer(Integer) -> list_to_binary(integer_to_list(Integer)).


%%%===================================================================
%%% Internal functions
%%%===================================================================





