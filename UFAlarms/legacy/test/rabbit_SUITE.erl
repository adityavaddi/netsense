-module(rabbit_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0
    ,groups/0
    ,init_per_suite/1
    ,end_per_suite/1
]).

-export([
    flapping/1
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
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Running tests for this suite
%% @end
%%--------------------------------------------------------------------
all() ->
    [{'group', 'rabbit'}].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Tests groups
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{'rabbit', [], ['flapping']}].


%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special init config for suite
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ct:pal("START OF ~p", [?MODULE]),
    test_helpers:init_legacy(),

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
flapping(_Config) ->
    test_helpers:mock_s3_worker(),
    test_helpers:mock_gproc(),

    'ok' = meck:new('rabbit_conn_worker', ['passthrough']),
    'ok' = meck:expect('rabbit_conn_worker', 'init', fun(_Args) ->
        ct:pal("rabbit_conn_worker:init(~p)", [_Args]),
        {'ok', EvtMgr} = gen_event:start_link({'local', 'rabbit_conn_evt_mgr'}),
        self() ! 'connect',
        {'ok', #state{exchanges=[]
                      ,timeout=5000
                      ,evtMgr=EvtMgr}}
    end),
    'ok' = meck:expect('rabbit_conn_worker', 'handle_cast',
        fun({'join_feed', Pid}, #state{evtMgr=EvtMgr, lastEvent=Event}=State) ->
            HandlerId = {'rabbit_feed', make_ref()},
            gen_event:add_handler(EvtMgr, HandlerId, [Pid]),
            Pid ! {?MODULE, {'joined', HandlerId}},
            'ok' = gen_event:notify(EvtMgr, Event),
            {'noreply', State};
        (_Msg, State) ->
            {'noreply', State}
        end
    ),
    'ok' = meck:expect('rabbit_conn_worker', 'handle_info',
        fun({'notify', Event}, #state{evtMgr=EvtMgr}=State) ->
            'ok' = gen_event:notify(EvtMgr, Event),
            {'noreply', State#state{lastEvent=Event}};
        ('connect', State) ->
            Conn = erlang:spawn('timer', 'sleep', [300]),
            _Ref = erlang:monitor('process', Conn),
            self() ! {'notify', {'ok', 'connected'}},
            {'noreply', State#state{connection=Conn}};
        ({'DOWN', Ref, 'process', Conn, Reason}, #state{connection=Conn}=State) ->
            self() ! {'notify', {'error', 'disconnected', Reason}},
            erlang:demonitor(Ref),
            self() ! 'connect',
            {'noreply', State#state{connection='undefined'}};
        (_Msg, State) ->
            ct:pal("~p", [_Msg]),
            {'noreply', State}
        end
    ),

    {'ok', _} = application:ensure_all_started('legacy'),
    'ok' =  meck:wait(1, 'rabbit_conn_worker', 'init', '_', 5000),

    'ok' = rabbit_conn_worker:join_feed(self()),
    rcv_loop('joined').


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
rcv_loop('joined') ->
    receive
        {'rabbit_SUITE', {'joined', {'rabbit_feed', _Ref}}} ->
            rcv_loop('connected')
    after 1000 ->
        ct:fail("did not get joined message")
    end;
rcv_loop('connected') ->
    receive
        {'rabbit_feed', {'ok', 'connected'}} ->
            rcv_loop('disconnected')
    after 1000 ->
        ct:fail("did not get connected message")
    end;
rcv_loop('disconnected') ->
    receive
        {'rabbit_feed', {'error', 'disconnected', 'normal'}} -> 'ok'
    after 1000 ->
        ct:fail("did not get disconnected message")
    end.
