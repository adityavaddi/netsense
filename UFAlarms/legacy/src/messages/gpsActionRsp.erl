%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == GPS Action Request ==
%% GPS Action Request message API
%% @end
%%%-------------------------------------------------------------------
-module(gpsActionRsp).

-export([
    to_map/1
    ,to_record/1
    ,create/3
    ,key/2
]).

-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type 'GpsActionRsp'() :: #'GpsActionRsp'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert GpsActionRsp record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('GpsActionRsp'()) -> map().
to_map(GAR) ->
    #{
        "name" => "GpsActionRsp"
        ,"actionType" => legacy_helpers:symbol_to_string(GAR#'GpsActionRsp'.actionType)
        ,"iValue" => legacy_helpers:symbol_to_string(GAR#'GpsActionRsp'.iValue)
        ,"sValue" => legacy_helpers:symbol_to_string(GAR#'GpsActionRsp'.sValue)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert GpsActionRsp record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'GpsActionRsp'().
to_record(Map) ->
    #'GpsActionRsp'{
        actionType = legacy_helpers:symbol_to_atom(maps:get("actionType", Map))
        ,iValue = legacy_helpers:undef_string_to_atom(maps:get("iValue", Map, 'undefined'))
        ,sValue = legacy_helpers:symbol_to_binary(maps:get("sValue", Map, 'undefined'), 'undefined')
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(atom(), integer() | 'undefined', binary() | 'undefined') -> 'GpsActionRsp'().
create(Action, I, S) ->
    #'GpsActionRsp'{
        actionType = Action
        ,iValue = I
        ,sValue = S
    }.

%%--------------------------------------------------------------------
%% @doc
%% Return routing key
%% @end
%%--------------------------------------------------------------------
-spec key(binary(), 'GpsActionRsp'()) -> binary().
key(NodeID, Map) when is_map(Map) ->
    Action = legacy_helpers:to_binary(maps:get("actionType", Map)),
    <<NodeID/binary, ".gps.", Action/binary>>;
key(NodeID, #'GpsActionRsp'{actionType=Action}) ->
    Bin = legacy_helpers:to_binary(Action),
    <<NodeID/binary, ".gps.", Bin/binary>>.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

to_map_test() ->
    GpsActionRsp = #'GpsActionRsp'{
        actionType = 'SendSample'
        ,iValue = 0
        ,sValue = <<"svalue">>
    },
    Map = #{
        "name" => "GpsActionRsp"
        ,"actionType" => "SendSample"
        ,"iValue" => 0
        ,"sValue" => "svalue"
    },
    ?assertEqual(Map, to_map(GpsActionRsp)),
    'ok'.

to_record_test() ->
    GpsActionRsp = #'GpsActionRsp'{
        actionType = 'SendSample'
        ,iValue = 0
        ,sValue = <<"svalue">>
    },
    Map = #{
        "name" => "GpsActionRsp"
        ,"actionType" => "SendSample"
        ,"iValue" => 0
        ,"sValue" => "svalue"
    },
    ?assertEqual(GpsActionRsp, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'GpsActionRsp'{
        actionType = 'SendSample'
        ,iValue = 0
        ,sValue = <<"svalue">>
    }, create('SendSample', 0, <<"svalue">>)).

key_test() ->
    GpsActionRsp = #'GpsActionRsp'{
        actionType = 'SendSample'
        ,iValue = 0
        ,sValue = <<"svalue">>
    },
    Map = #{
        "name" => "GpsActionRsp"
        ,"actionType" => "SendSample"
        ,"iValue" => 0
        ,"sValue" => "svalue"
    },
    ?assertEqual(<<"J007.gps.SendSample">>, key(<<"J007">>, GpsActionRsp)),
    ?assertEqual(<<"J007.gps.SendSample">>, key(<<"J007">>, Map)).

-endif.
