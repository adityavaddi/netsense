-module(test_helpers).

-include_lib("common_test/include/ct.hrl").

-export([
    init_legacy/0, stop_legacy/0
    ,mock_deps/0, unmock/0
    ,mock_rabbit/0
    ,mock_s3_worker/0
    ,mock_gproc/0
    ,mock_rabbit_helper/0
    ,expect_published/1
]).

-record(state, {
    connection
    ,params
    ,exchanges
    ,timeout
    ,evtMgr
    ,lastEvent
}).


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_legacy() ->
    lists:foreach(
        fun({Key, Val}) ->
            application:set_env('legacy', Key, Val)
        end
        ,ct:get_config('legacy', [])
    ),
    lists:foreach(
        fun({Key, Val}) ->
            application:set_env('lager', Key, Val)
        end
        ,ct:get_config('lager', [])
    ),
    {'ok', _} = application:ensure_all_started('meck'),
    {'ok', _} = application:ensure_all_started('gun').

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
stop_legacy() ->
    application:stop('legacy'),
    application:stop('gun'),
    application:stop('meck').

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
mock_deps() ->
    _ = mock_gproc(),
    _ = mock_rabbit(),
    _ = mock_s3_worker(),
    mock_legacy_metrics().

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
unmock() ->
    meck:unload().

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
mock_rabbit() ->
    'ok' = meck:new('rabbit_conn_worker', ['passthrough']),
    'ok' = meck:new('rabbit_conn_watcher', ['passthrough']),

    'ok' = meck:expect('rabbit_conn_worker', 'init', fun(_Args) ->
        ct:pal("rabbit_conn_worker:init(~p)", [_Args]),
        {'ok', EvtMgr} = gen_event:start_link({'local', 'rabbit_conn_evt_mgr'}),
        {'ok', #state{exchanges=[]
                      ,timeout=5000
                      ,evtMgr=EvtMgr
                      ,lastEvent={'ok', 'connected'}}}
    end),
    'ok' = meck:expect('rabbit_conn_worker', 'handle_cast',
        fun({'join_feed', Pid}, #state{evtMgr=EvtMgr, lastEvent=Event}=State) ->
            HandlerId = {'rabbit_feed', make_ref()},
            lager:info("~p joined feed", [Pid]),
            gen_event:add_handler(EvtMgr, HandlerId, [Pid]),
            Pid ! {'rabbit_conn_worker', {'joined', HandlerId}},
            'ok' = gen_event:notify(EvtMgr, Event),
            {'noreply', State};
        (_Msg, State) ->
             {'noreply', State}
        end
    ),
    'ok' = meck:expect('rabbit_conn_watcher', 'handle_info',
        fun(_Msg, State) ->
            {'noreply', State}
        end
    ),
    ct:pal("rabbit_conn_worker mocked").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
mock_s3_worker() ->
    'ok' = meck:new('legacy_s3_worker', ['passthrough']),
    'ok' = meck:expect('legacy_s3_worker', 'init', fun(_Args) ->
        ct:pal(" legacy_s3_worker:init(~p)", [_Args]),
        {'ok', []}
    end),
    ct:pal("legacy_s3_worker mocked").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
mock_legacy_metrics() ->
    'ok' = meck:new('legacy_metrics', ['passthrough']),
    'ok' = meck:expect('legacy_metrics', 'init', fun(_Args) ->
        ct:pal(" legacy_metrics:init(~p)", [_Args]),
        {'ok', []}
    end),
    ct:pal("legacy_metrics mocked").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
mock_gproc() ->
    'ok' = meck:new('gproc', ['no_link', 'passthrough']),
    'ok' = meck:expect('gproc', 'lookup_global_name', fun(Name) ->
        ct:pal(" gproc:lookup_global_name(~p)", [Name]),
        gproc:lookup_local_name(Name)
    end),
    'ok' = meck:expect('gproc', 'add_global_name', fun(Name) ->
        ct:pal(" gproc:add_global_name(~p)", [Name]),
        gproc:add_local_name(Name)
    end),
    ct:pal("gproc mocked").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
mock_rabbit_helper() ->
    'ok' = meck:new('rabbit_helper', ['no_link']),
    'ok' = meck:expect('rabbit_helper', 'publish', fun(X, K, P) ->
        {'ok', M} = msgpack:unpack(P),
        ct:pal("rabbit_helper:publish(~p, ~p, ~p)", [X, K, M]),
        'ok'
    end),
    ct:pal("rabbit_helper mocked").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
expect_published(Expected) ->
    Length = erlang:length(Expected),
    'ok' =  meck:wait(Length, 'rabbit_helper', 'publish', '_', 1000 * Length),
    History = meck:history('rabbit_helper'),
    NewHistory = lists:foldr(
        fun({_, {'rabbit_helper', 'publish', [X, K, P]}, _}, Acc) ->
            {'ok', M} = msgpack:unpack(P),
            [{X, K, M}|Acc]
        end
        ,[]
        ,History
    ),
    expect_published(NewHistory, Expected),
    'ok'.

expect_published([], []) -> 'ok';
expect_published([{X, K, M}|History], [{X, K, M}|Expected]) ->
    expect_published(History, Expected);
% This is just in case message are send in at the same time
expect_published([{X, K, M}|History], [{X1, K1, M1}, {X, K, M}|Expected]) ->
    expect_published(History, [{X1, K1, M1}|Expected]);
expect_published([{X, K, M}|_History], [{X1, K1, M1}|_Expected]) ->
    ct:fail("Expected: ~p Got: ~p", [{X1, K1, M1}, {X, K, M}]).
%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
