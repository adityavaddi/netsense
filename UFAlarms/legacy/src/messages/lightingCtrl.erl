%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Lighting Control ==
%% Lighting Control message API
%% @end
%%%-------------------------------------------------------------------
-module(lightingCtrl).

-export([
    to_map/1
    ,to_record/1
    ,create/4
    ,key/2
]).

-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type 'LightingCtrl'() :: #'LightingCtrl'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert LightingCtrl record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('LightingCtrl'()) -> map().
to_map(LC) ->
    #{
        "name" => "LightingCtrl"
        ,"pri" => LC#'LightingCtrl'.pri
        ,"mask" => LC#'LightingCtrl'.mask
        ,"level" => LC#'LightingCtrl'.level
        ,"qualifiers" => LC#'LightingCtrl'.qualifiers
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert LightingCtrl record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'LightingCtrl'().
to_record(Map) ->
    #'LightingCtrl'{
        pri = maps:get("pri", Map)
        ,mask = maps:get("mask", Map)
        ,level = <<(maps:get("level", Map))>>
        ,qualifiers = legacy_helpers:undef_string_to_atom(maps:get("qualifiers", Map, 'undefined'))
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(integer(), integer(), integer(), integer() | 'undefined') -> 'LightingCtrl'().
create(Pri, Mask, Lvl, Q) ->
    #'LightingCtrl'{
        pri = Pri
        ,mask = Mask
        ,level = <<(Lvl)>>
        ,qualifiers = Q
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
    LightingCtrl = #'LightingCtrl'{
        pri = 1
        ,mask = 55
        ,level = <<0>>
        ,qualifiers = 12
    },
    Map = #{
        "name" => "LightingCtrl"
        ,"pri" => 1
        ,"mask" => 55
        ,"level" => <<0>>
        ,"qualifiers" => 12
    },
    ?assertEqual(Map, to_map(LightingCtrl)),
    'ok'.

to_record_test() ->
    LightingCtrl = #'LightingCtrl'{
        pri = 1
        ,mask = 55
        ,level = <<0>>
        ,qualifiers = 12
    },
    Map = #{
        "name" => "LightingCtrl"
        ,"pri" => 1
        ,"mask" => 55
        ,"level" => 0
        ,"qualifiers" => 12
    },
    ?assertEqual(LightingCtrl, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'LightingCtrl'{
        pri = 1
        ,mask = 55
        ,level = <<0>>
        ,qualifiers = 12
    }, create(1, 55, 0, 12)).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
