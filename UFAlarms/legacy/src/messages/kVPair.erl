%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Key Value Pair ==
%% Key Value Pair message API
%% @end
%%%-------------------------------------------------------------------
-module(kVPair).

-export([
    to_map/1
    ,to_record/1
    ,create/2
    ,key/2
]).

-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type 'KVPair'() :: #'KVPair'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert KVPair record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('KVPair'()) -> map().
to_map(Pair) ->
    #{
        "name" => "KVPair"
        ,"key" => legacy_helpers:symbol_to_string(Pair#'KVPair'.key)
        ,"sValue" => legacy_helpers:symbol_to_string(Pair#'KVPair'.sValue)
        ,"lValue" => legacy_helpers:undef_atom_to_string(Pair#'KVPair'.lValue)
        ,"bValue" => legacy_helpers:undef_atom_to_string(Pair#'KVPair'.bValue)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert KVPair record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'KVPair'().
to_record(Map) ->
    #'KVPair'{
        key = legacy_helpers:symbol_to_binary(maps:get("key", Map))
        ,sValue = legacy_helpers:symbol_to_binary(maps:get("sValue", Map, 'undefined'))
        ,lValue = maps:get("lValue", Map, 'undefined')
        ,bValue = maps:get("bValue", Map, 'undefined')
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(string(), any()) -> 'KVPair'().
create(Key, Value) when is_list(Value) ->
    #'KVPair'{
        key = legacy_helpers:symbol_to_binary(Key)
        ,sValue = legacy_helpers:symbol_to_binary(Value)
        ,lValue = 'undefined'
        ,bValue = 'undefined'
    };
create(Key, Value) when is_binary(Value) ->
    #'KVPair'{
        key = legacy_helpers:symbol_to_binary(Key)
        ,sValue = Value
        ,lValue = 'undefined'
        ,bValue = 'undefined'
    };
create(Key, Value) when is_number(Value) ->
    #'KVPair'{
        key = legacy_helpers:symbol_to_binary(Key)
        ,sValue = 'undefined'
        ,lValue = Value
        ,bValue = 'undefined'
    };
create(Key, 'true') ->
    #'KVPair'{
        key = legacy_helpers:symbol_to_binary(Key)
        ,sValue = 'undefined'
        ,lValue = 'undefined'
        ,bValue = 'true'
    };
create(Key, 'false') ->
    #'KVPair'{
        key = legacy_helpers:symbol_to_binary(Key)
        ,sValue = 'undefined'
        ,lValue = 'undefined'
        ,bValue = 'false'
    }.

%%--------------------------------------------------------------------
%% @doc
%% Return routing key
%% @end
%%--------------------------------------------------------------------
-spec key(binary(), any()) -> binary().
key(_NodeID, _) -> <<>>.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

to_map_test() ->
    KVPair = #'KVPair'{
        key = <<"key">>
        ,sValue = <<"value">>
        ,lValue = 'undefined'
        ,bValue = 'undefined'
    },
    Map = #{
        "name" => "KVPair"
        ,"key" => "key"
        ,"sValue" => "value"
        ,"lValue" => "undefined"
        ,"bValue" => "undefined"
    },
    ?assertEqual(Map, to_map(KVPair)),
    'ok'.

to_record_test() ->
    KVPair = #'KVPair'{
        key = <<"key">>
        ,sValue = <<"value">>
        ,lValue = 1
        ,bValue = 'true'
    },
    Map = #{
        "name" => "KVPair"
        ,"key" => "key"
        ,"sValue" => "value"
        ,"lValue" => 1
        ,"bValue" => 'true'
    },
    ?assertEqual(KVPair, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'KVPair'{key = <<"key">>, sValue= <<"str">>}, create("key", "str")),
    ?assertMatch(#'KVPair'{key = <<"key">>, sValue= <<"bin">>}, create("key", <<"bin">>)),
    ?assertMatch(#'KVPair'{key = <<"key">>, lValue= 1}, create("key", 1)),
    ?assertMatch(#'KVPair'{key = <<"key">>, bValue= 'true'}, create("key", 'true')),
    ?assertMatch(#'KVPair'{key = <<"key">>, bValue= 'false'}, create("key", 'false')).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
