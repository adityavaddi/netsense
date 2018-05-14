%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Config Response ==
%% Config Response message API
%% @end
%%%-------------------------------------------------------------------
-module(configResp).

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

-type 'ConfigResp'() :: #'ConfigResp'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert ConfigResp record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('ConfigResp'()) -> map().
to_map(CR) ->
    #{
        "name" => "ConfigResp"
        ,"pair" => kVPair:to_map(CR#'ConfigResp'.pair)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert ConfigResp record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'ConfigResp'().
to_record(Map) ->
    #'ConfigResp'{
        pair = kVPair:to_record(maps:get("pair", Map))
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(string(), any()) -> 'ConfigResp'().
create(Key, Value) ->
    #'ConfigResp'{
        pair = kVPair:create(Key, Value)
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
    ConfigResp = #'ConfigResp'{
        pair = #'KVPair'{
            key = <<"key">>
            ,sValue = <<"value">>
            ,lValue = 1
            ,bValue = 'true'
        }
    },
    Map = #{
        "name" => "ConfigResp"
        ,"pair" => #{
            "name" => "KVPair"
            ,"key" => "key"
            ,"sValue" => "value"
            ,"lValue" => 1
            ,"bValue" => 'true'
        }
    },
    ?assertEqual(Map, to_map(ConfigResp)),
    'ok'.

to_record_test() ->
    ConfigResp = #'ConfigResp'{
        pair = #'KVPair'{
            key = <<"key">>
            ,sValue = <<"value">>
            ,lValue = 1
            ,bValue = 'true'
        }
    },
    Map = #{
        "name" => "ConfigResp"
        ,"pair" => #{
            "name" => "KVPair"
            ,"key" => "key"
            ,"sValue" => "value"
            ,"lValue" => 1
            ,"bValue" => 'true'
        }
    },
    ?assertEqual(ConfigResp, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'ConfigResp'{pair=#'KVPair'{key = <<"key">>, lValue = 1}}, create("key", 1)).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
