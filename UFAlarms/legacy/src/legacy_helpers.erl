%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Legacy Helpers ==
%% Series of helper functions
%% @end
%%%-------------------------------------------------------------------
-module(legacy_helpers).

-export([
    init_lager/1
    ,time/0
    ,symbol_to_string/1
    ,symbol_to_binary/1 ,symbol_to_binary/2
    ,symbol_to_atom/1
    ,undef_atom_to_string/1
    ,undef_string_to_atom/1
    ,to_binary/1
    ,str_match/2
    ,filter_offline_nodes/1
    ,node_online/1
    ,node_key/1
    ,filter_by/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%--------------------------------------------------------------------
%% @doc
%% Add variable `nodeid' to lager
%% @end
%%--------------------------------------------------------------------
-spec init_lager(any()) -> any().
init_lager(Value) ->
    lager:md([{'nodeid', symbol_to_string(Value)}]).

%%--------------------------------------------------------------------
%% @doc
%% Get OS timestamp
%% @end
%%--------------------------------------------------------------------
-spec time() -> integer().
time() ->
    os:system_time('micro_seconds').

%%--------------------------------------------------------------------
%% @doc
%% Take a variable and convert it to `string()' if `atom()' or `binary()'
%% @end
%%--------------------------------------------------------------------
-spec symbol_to_string(any()) -> any().
symbol_to_string(Bin) when is_binary(Bin)->
    binary:bin_to_list(Bin);
symbol_to_string(Atom) when is_atom(Atom)->
    erlang:atom_to_list(Atom);
symbol_to_string(Any)->
    Any.

%%--------------------------------------------------------------------
%% @doc
%% Take a variable and convert it to `binary()' if `atom()' or `string()'
%% @end
%%--------------------------------------------------------------------
-spec symbol_to_binary(any()) -> any().
symbol_to_binary(Atom) when is_atom(Atom)->
    erlang:atom_to_binary(Atom, 'utf8');
symbol_to_binary(Str) when is_list(Str)->
    binary:list_to_bin(Str);
symbol_to_binary(Any)->
    Any.

%%--------------------------------------------------------------------
%% @doc
%% Take a variable and convert it to `binary()' if `atom()' or `string()'
%% except if value = default
%% @end
%%--------------------------------------------------------------------
-spec symbol_to_binary(any(), any()) -> any().
symbol_to_binary(Value, Value) -> Value;
symbol_to_binary(Value, _Default) ->
    symbol_to_binary(Value).

%%--------------------------------------------------------------------
%% @doc
%% Take a variable and convert it to `atom()' if `string()' or `binary()'
%% @end
%%--------------------------------------------------------------------
-spec symbol_to_atom(any()) -> any().
symbol_to_atom(List) when is_list(List)->
    erlang:list_to_existing_atom(List);
symbol_to_atom(Bin) when is_binary(Bin)->
    erlang:binary_to_atom(Bin, 'utf8');
symbol_to_atom(Any)->
    Any.

%%--------------------------------------------------------------------
%% @doc
%% Convert 'undefined' to string
%% @end
%%--------------------------------------------------------------------
-spec undef_atom_to_string(any()) -> any().
undef_atom_to_string('undefined') -> "undefined";
undef_atom_to_string(Any)->
    Any.

%%--------------------------------------------------------------------
%% @doc
%% Convert 'undefined' to string
%% @end
%%--------------------------------------------------------------------
-spec undef_string_to_atom(any()) -> any().
undef_string_to_atom("undefined") -> 'undefined';
undef_string_to_atom(Any)->
    Any.

%%--------------------------------------------------------------------
%% @doc
%% Take a variable and convert it to `binary()'
%% @end
%%--------------------------------------------------------------------
-spec to_binary(any()) -> binary().
to_binary(Atom) when is_atom(Atom)->
    erlang:atom_to_binary(Atom, 'utf8');
to_binary(Str) when is_list(Str)->
     binary:list_to_bin(Str);
to_binary(Number) when is_number(Number)->
    erlang:integer_to_binary(Number);
to_binary(Binary) when is_binary(Binary)->
    Binary.

%%--------------------------------------------------------------------
%% @doc
%% Return `true' if `Match' is found in `Subject' otherwise return `false'
%% @end
%%--------------------------------------------------------------------
-spec str_match('undefined' | string(), string()) -> boolean().
str_match('undefined', _Match) -> 'false';
str_match(Subject, Match) ->
    case string:str(Subject, Match) of
        0 -> 'false';
        _ -> 'true'
    end.

%%--------------------------------------------------------------------
%% @doc
%% Filter a `list()' of Node IDs (if Node is online)
%% @end
%%--------------------------------------------------------------------
-spec filter_offline_nodes([string(), ...]) -> [{string(), pid()}, ...].
filter_offline_nodes(NodeIDs) ->
    lists:foldl(
        fun(NodeID, Acc) ->
            case node_online(NodeID) of
                'false' ->
                    lager:debug("did not find any connection for ~p", [NodeID]),
                    Acc;
                {'true', Pid} ->
                    [{NodeID, Pid}|Acc]
            end
        end
        ,[]
        ,NodeIDs
    ).

%%--------------------------------------------------------------------
%% @doc
%% Take a Node ID return `true' if online `false' if offline
%% @end
%%--------------------------------------------------------------------
-spec node_online(string() |  binary()) -> 'false' | {'true', pid()}.
node_online(NodeID) ->
    case gproc:lookup_global_name(?MODULE:node_key(NodeID)) of
        'undefined' -> 'false';
        Pid -> {'true', Pid}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Return gproc node key
%% @end
%%--------------------------------------------------------------------
-spec node_key(string() | binary()) -> {'node', binary()}.
node_key(NodeID) ->
    {'node', legacy_helpers:symbol_to_binary(NodeID)}.

%%--------------------------------------------------------------------
%% @doc
%% Take a `list()' of string and filter it by `Match' `string()'
%% @end
%%--------------------------------------------------------------------
-spec filter_by([string(), ...], string()) -> [string(), ...] | [].
filter_by(Strings, Match) ->
    lists:filtermap(
        fun(String) ->
            legacy_helpers:str_match(String, Match)
        end
        ,Strings
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

symbol_to_string_test() ->
    ?assertEqual("ok", legacy_helpers:symbol_to_string('ok')),
    ?assertEqual("ok", legacy_helpers:symbol_to_string("ok")),
    ?assertEqual("ok", legacy_helpers:symbol_to_string(<<"ok">>)),
    ?assertEqual(12, legacy_helpers:symbol_to_string(12)),
    'ok'.

symbol_to_binary_test() ->
    ?assertEqual(<<"ok">>, legacy_helpers:symbol_to_binary('ok')),
    ?assertEqual(<<"ok">>, legacy_helpers:symbol_to_binary("ok")),
    ?assertEqual(<<"ok">>, legacy_helpers:symbol_to_binary(<<"ok">>)),
    ?assertEqual(12, legacy_helpers:symbol_to_binary(12)),
    'ok'.

symbol_to_atom_test() ->
    ?assertEqual('ok', legacy_helpers:symbol_to_atom('ok')),
    ?assertEqual('ok', legacy_helpers:symbol_to_atom("ok")),
    ?assertEqual('ok', legacy_helpers:symbol_to_atom(<<"ok">>)),
    ?assertEqual(12, legacy_helpers:symbol_to_atom(12)),
    'ok'.

to_binary_test() ->
    ?assertEqual(<<"ok">>, legacy_helpers:to_binary('ok')),
    ?assertEqual(<<"ok">>, legacy_helpers:to_binary("ok")),
    ?assertEqual(<<"ok">>, legacy_helpers:to_binary(<<"ok">>)),
    ?assertEqual(<<"12">>, legacy_helpers:to_binary(12)),
    ?assertError('function_clause', legacy_helpers:to_binary(self())),
    'ok'.

filter_by_test() ->
    ?assertEqual(["1", "11"], filter_by(["1", "11", "2"], "1")),
    'ok'.

-endif.
