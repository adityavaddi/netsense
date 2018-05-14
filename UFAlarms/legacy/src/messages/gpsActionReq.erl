%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == GPS Action Request ==
%% GPS Action Request message API
%% @end
%%%-------------------------------------------------------------------
-module(gpsActionReq).

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

-type 'GpsActionReq'() :: #'GpsActionReq'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert GpsActionReq record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('GpsActionReq'()) -> map().
to_map(GAR) ->
    #{
        "name" => "GpsActionReq"
        ,"actionType" => legacy_helpers:symbol_to_string(GAR#'GpsActionReq'.actionType)
        ,"iValue" => legacy_helpers:symbol_to_string(GAR#'GpsActionReq'.iValue)
        ,"sValue" => legacy_helpers:symbol_to_string(GAR#'GpsActionReq'.sValue)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert GpsActionReq record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'GpsActionReq'().
to_record(Map) ->
    #'GpsActionReq'{
        actionType = legacy_helpers:symbol_to_atom(maps:get("actionType", Map))
        ,iValue = legacy_helpers:undef_string_to_atom(maps:get("iValue", Map, 'undefined'))
        ,sValue = legacy_helpers:symbol_to_binary(maps:get("sValue", Map, 'undefined'), 'undefined')
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(atom(), integer() | 'undefined', binary() | 'undefined') -> 'GpsActionReq'().
create(Action, I, S) ->
    #'GpsActionReq'{
        actionType = Action
        ,iValue = I
        ,sValue = S
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
    GpsActionReq = #'GpsActionReq'{
        actionType = 'SendSample'
        ,iValue = 0
        ,sValue = <<"svalue">>
    },
    Map = #{
        "name" => "GpsActionReq"
        ,"actionType" => "SendSample"
        ,"iValue" => 0
        ,"sValue" => "svalue"
    },
    ?assertEqual(Map, to_map(GpsActionReq)),
    'ok'.

to_record_test() ->
    GpsActionReq = #'GpsActionReq'{
        actionType = 'SendSample'
        ,iValue = 0
        ,sValue = <<"svalue">>
    },
    Map = #{
        "name" => "GpsActionReq"
        ,"actionType" => "SendSample"
        ,"iValue" => 0
        ,"sValue" => "svalue"
    },
    ?assertEqual(GpsActionReq, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'GpsActionReq'{
        actionType = 'SendSample'
        ,iValue = 0
        ,sValue = <<"svalue">>
    }, create('SendSample', 0, <<"svalue">>)).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
