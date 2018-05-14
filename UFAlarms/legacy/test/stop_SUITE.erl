-module(stop_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("messages_pb.hrl").
-export([
    all/0
    ,groups/0
    ,init_per_suite/1
    ,end_per_suite/1
]).

-export([
    graceful_stop/1
]).

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
    [{'group', 'graceful_stop'}].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Tests groups
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{'graceful_stop', [], ['graceful_stop']}].


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
graceful_stop(_Config) ->
    test_helpers:mock_rabbit_helper(),
    'ok' = simulate_nodes(10),
    application:stop('legacy'),
    meck:num_calls('rabbit_helper', 'publish', '_', 10).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
simulate_nodes(0) -> 'ok';
simulate_nodes(Int) ->
    {'ok', ConnPid} = gun:open("localhost", 8080),
    gun:ws_upgrade(ConnPid, "/device/N_" ++ erlang:integer_to_list(Int)),
    timer:sleep(10),
    rcv_loop(),
    ct:pal("connected node N_~s", [erlang:integer_to_list(Int)]),
    simulate_nodes(Int - 1).

rcv_loop() ->
    receive
        {'gun_up', _ConnPid, 'http'} ->
            rcv_loop();
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
