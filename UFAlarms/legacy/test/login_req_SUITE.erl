-module(login_req_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("messages_pb.hrl").
-export([
    all/0
    ,groups/0
    ,init_per_suite/1
    ,end_per_suite/1
]).

-export([
    login_req/1
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
    [{'group', 'login_req'}].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Tests groups
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{'login_req', [], ['login_req']}].


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
%%   Check login req received and published to rabbitmq
%% @end
%%--------------------------------------------------------------------
login_req(_Config) ->
    test_helpers:mock_rabbit_helper(),
    simulate_node(),
    test_helpers:expect_published([
        {<<"node.events">>, <<?NODEID/binary, ".login.req">>, loginReq:test_map(?NODEID)}
        ,{<<"node.events">>, <<?NODEID/binary, ".login.connected">>, connection_status:test_map(?NODEID, "connected")}
        ,{<<"node.events">>, <<?NODEID/binary, ".login.disconnected">>, connection_status:test_map(?NODEID, "disconnected")}
    ]).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
simulate_node() ->
    meck:new('legacy_helpers', ['passthrough']),
    meck:expect('legacy_helpers', 'time', fun() -> 0 end),
    {'ok', ConnPid} = gun:open("localhost", 8080),
    gun:ws_upgrade(ConnPid, "/device/" ++ legacy_helpers:symbol_to_string(?NODEID)),
    'ok' = login_req_loop(),
    timer:sleep(100),
    gun:close(ConnPid),
    ct:pal("closed cconnection ~p", [ConnPid]),
    meck:unload('legacy_helpers').

%%--------------------------------------------------------------------
%% @private
%% @doc
%%   Check if legacy websocket server is started and sends login req
%%   Also check for login resp
%% @end
%%--------------------------------------------------------------------
login_req_loop() ->
    receive
        {'gun_up', _ConnPid, 'http'} ->
            login_req_loop();
        {'gun_ws_upgrade', ConnPid, 'ok', _Header} ->
            LR = loginReq:test_record(?NODEID),
            Envelope = envelope:create(LR),
            Bin = envelope:encode(Envelope),
            'ok' = gun:ws_send(ConnPid, {'binary', Bin}),
            login_req_loop();
        {'gun_error', _ConnPid, _StreamRef, Reason} ->
            ct:fail(Reason);
        {'gun_ws', _ConnPid, {'binary', Msg}} ->
            Envelope = envelope:decode(Msg),
            LResp = loginResp:create(0),
            {'loginResp', LResp} = envelope:type(Envelope),
            ct:pal("got login resp message ~p", [LResp]),
            'ok';
        _Msg ->
            ct:pal("unhandled message ~p", [_Msg]),
            ct:fail("unhandled message")
    after 1000 ->
        ct:fail("timeout")
    end.
