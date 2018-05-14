%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Device Action Request ==
%% Device Action Request message API
%% @end
%%%-------------------------------------------------------------------
-module(deviceActionReq).

-export([
    to_map/1
    ,to_record/1
    ,create/1
    ,key/2
]).

-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type 'DeviceActionReq'() :: #'DeviceActionReq'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert DeviceActionReq record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('DeviceActionReq'()) -> map().
to_map(DAR) ->
    #{
        "name" => "DeviceActionReq"
        ,"actionType" => legacy_helpers:symbol_to_string(DAR#'DeviceActionReq'.actionType)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert DeviceActionReq record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'DeviceActionReq'().
to_record(Map) ->
    #'DeviceActionReq'{
        actionType = legacy_helpers:symbol_to_atom(maps:get("actionType", Map))
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(atom()) -> 'DeviceActionReq'().
create(Action) ->
    #'DeviceActionReq'{actionType=Action}.

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
    DeviceActionReq = #'DeviceActionReq'{
        actionType = 'ColdReset'
    },
    Map = #{
        "name" => "DeviceActionReq"
        ,"actionType" => "ColdReset"
    },
    ?assertEqual(Map, to_map(DeviceActionReq)),
    'ok'.

to_record_test() ->
    DeviceActionReq = #'DeviceActionReq'{
        actionType = 'ColdReset'
    },
    Map = #{
        "name" => "DeviceActionReq"
        ,"actionType" => "ColdReset"
    },
    ?assertEqual(DeviceActionReq, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'DeviceActionReq'{
        actionType = 'ColdReset'
    }, create('ColdReset')).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
