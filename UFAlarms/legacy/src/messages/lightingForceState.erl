%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Lighting Force State ==
%% Lighting Force State message API
%% @end
%%%-------------------------------------------------------------------
-module(lightingForceState).

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

-type 'LightingForceState'() :: #'LightingForceState'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert LightingForceState record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('LightingForceState'()) -> map().
to_map(LFS) ->
    #{
        "name" => "LightingForceState"
        ,"state" => lightingCtrl:to_map(LFS#'LightingForceState'.state)
        ,"ftype" => legacy_helpers:symbol_to_string(LFS#'LightingForceState'.ftype)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert LightingForceState record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'LightingForceState'().
to_record(Map) ->
    #'LightingForceState'{
        state = lightingCtrl:to_record(maps:get("state", Map))
        ,ftype = legacy_helpers:symbol_to_atom(maps:get("ftype", Map, 'undefined'))
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(any(), atom()) -> 'LightingForceState'().
create(State, FType) ->
    #'LightingForceState'{
        state = State
        ,ftype = FType
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
    LightingForceState = #'LightingForceState'{
        state = #'LightingCtrl'{
            pri = 1
            ,mask = 55
            ,level = <<0>>
            ,qualifiers = 12
        }
        ,ftype = 'Persistent'
    },
    Map = #{
        "name" => "LightingForceState"
        ,"state" => #{
            "name" => "LightingCtrl"
            ,"pri" => 1
            ,"mask" => 55
            ,"level" => <<0>>
            ,"qualifiers" => 12
        }
        ,"ftype" => "Persistent"
    },
    ?assertEqual(Map, to_map(LightingForceState)),
    'ok'.

to_record_test() ->
    LightingForceState = #'LightingForceState'{
        state = #'LightingCtrl'{
            pri = 1
            ,mask = 55
            ,level = <<0>>
            ,qualifiers = 12
        }
        ,ftype = 'Persistent'
    },
    Map = #{
        "name" => "LightingForceState"
        ,"state" => #{
            "name" => "LightingCtrl"
            ,"pri" => 1
            ,"mask" => 55
            ,"level" => 0
            ,"qualifiers" => 12
        }
        ,"ftype" => "Persistent"
    },
    ?assertEqual(LightingForceState, to_record(Map)),
    'ok'.

create_test() ->
    State = lightingCtrl:create(1, 55, 0, 12),
    ?assertMatch(#'LightingForceState'{
        state = #'LightingCtrl'{
            pri = 1
            ,mask = 55
            ,level = <<0>>
            ,qualifiers = 12
        }
        ,ftype = 'Persistent'
    }, create(State, 'Persistent')).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
