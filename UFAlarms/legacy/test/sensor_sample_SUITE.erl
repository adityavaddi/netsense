-module(sensor_sample_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("messages_pb.hrl").
-export([
    all/0
    ,groups/0
    ,init_per_suite/1
    ,end_per_suite/1
]).

-export([
    lt/1
]).

-define(NODEID, <<"J007">>).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Running tests for this suite
%% @end
%%--------------------------------------------------------------------
all() ->
    [{'group', 'sensor_sample'}].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Tests groups
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{'sensor_sample', [], ['lt']}].


%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special init config for suite
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ct:pal("START OF ~p", [?MODULE]),

    test_helpers:init_legacy(),
    test_helpers:mock_deps(),

    {'ok', _} = application:ensure_all_started('legacy'),

    Config.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special end config for groups
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    test_helpers:stop_legacy(),
    test_helpers:unmock(),
    ct:pal("END OF ~p", [?MODULE]),
    'ok'.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Check if legacy application is started
%% @end
%%--------------------------------------------------------------------
lt(_Config) ->
    test_helpers:mock_rabbit_helper(),

    SS = sensorSample:create(<<"lt">>, 1485213091784285, 7, <<"lux">>),
    Map = sensorSample:to_map(SS),
    Envelope = envelope:create(SS),
    Encoded = envelope:encode(Envelope),

    Result = maps:put("nodeid", erlang:binary_to_list(?NODEID), Map),
    ConnPid = send_payload(Encoded),
    test_helpers:expect_published([
        {<<"node.events">>, <<?NODEID/binary, ".login.connected">>, connection_status:test_map(?NODEID, "connected")}
        ,{<<"node.events">>, <<?NODEID/binary, ".sensor.lt">>, Result}
    ]),
    gun:close(ConnPid),
    'ok'.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%%   Open ws connection, send message and close connection
%% @end
%%--------------------------------------------------------------------
send_payload(Bin) ->
    {'ok', ConnPid} = gun:open("localhost", 8080),
    gun:ws_upgrade(ConnPid, "/device/" ++ erlang:binary_to_list(?NODEID)),
    connection_rcv(),
    'ok' = gun:ws_send(ConnPid, {'binary', Bin}),
    ConnPid.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%   Check if legacy websocket server is started
%% @end
%%--------------------------------------------------------------------
connection_rcv() ->
    receive
        {'gun_up', _ConnPid, 'http'} ->
            connection_rcv();
        {'gun_ws_upgrade', _ConnPid, 'ok', _Header} ->
            'ok';
        {'gun_error', _ConnPid, _StreamRef, Reason} ->
            ct:fail(Reason);
        _Msg ->
            ct:pal("unhandled message ~p", [_Msg]),
            ct:fail("unhandled message")
    after 1000 ->
        ct:fail("timeout")
    end.
