%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == ConnectionStatus ==
%% Connection Status message API
%% @end
%%%-------------------------------------------------------------------
-module(connection_status).

-export([
    to_map/1
    ,to_record/1
    ,disconnected/0
    ,connected/0
    ,key/2
]).

-export([
    test_map/2
]).

-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record('ConnectionStatus',
    {status :: string()
}).

-type 'ConnectionStatus'() :: #'ConnectionStatus'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert ConnectionStatus record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('ConnectionStatus'()) -> map().
to_map(CS) ->
    #{
        "name" => "ConnectionStatus"
        ,"status" => legacy_helpers:symbol_to_string(CS#'ConnectionStatus'.status)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert ConnectionStatus record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'ConnectionStatus'().
to_record(Map) ->
    #'ConnectionStatus'{
        status = legacy_helpers:symbol_to_string(maps:get("status", Map))
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec connected() -> 'ConnectionStatus'().
connected() ->
    #'ConnectionStatus'{status = "connected"}.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec disconnected() -> 'ConnectionStatus'().
disconnected() ->
    #'ConnectionStatus'{status = "disconnected"}.

%%--------------------------------------------------------------------
%% @doc
%% Return routing key
%% @end
%%--------------------------------------------------------------------
-spec key(binary(), 'ConnectionStatus'()) -> binary().
key(NodeID, CS) ->
    Status = legacy_helpers:symbol_to_binary(CS#'ConnectionStatus'.status),
    <<NodeID/binary, ".login.", Status/binary>>.

%%--------------------------------------------------------------------
%% @doc
%% Return test Login Req map
%% @end
%%--------------------------------------------------------------------
-spec test_map(binary() | string(), binary() | string()) -> map().
test_map(NodeID, Status) ->
    #{
        "name" => "ConnectionStatus"
        ,"nodeid" => legacy_helpers:symbol_to_string(NodeID)
        ,"status" => legacy_helpers:symbol_to_string(Status)
    }.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

to_map_test() ->
    ConnectionStatus = #'ConnectionStatus'{status = "disconnected"},
    Map = #{
        "name" => "ConnectionStatus"
        ,"status" => "disconnected"
    },
    ?assertEqual(Map, to_map(ConnectionStatus)),
    'ok'.

to_record_test() ->
    ConnectionStatus = #'ConnectionStatus'{status = "disconnected"},
    Map = #{
        "name" => "ConnectionStatus"
        ,"status" => "disconnected"
    },
    ?assertEqual(ConnectionStatus, to_record(Map)),
    'ok'.

disconnected_test() ->
    ?assertMatch(#'ConnectionStatus'{status = "disconnected"}, disconnected()).

connected_test() ->
    ?assertMatch(#'ConnectionStatus'{status = "connected"}, connected()).

key_test() ->
    ?assertEqual(<<"J007.login.disconnected">>, key(<<"J007">>, disconnected())),
    ?assertEqual(<<"J007.login.connected">>, key(<<"J007">>, connected())).

-endif.
