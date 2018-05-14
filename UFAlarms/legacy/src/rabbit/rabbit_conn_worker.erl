%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == RabbitMQ Connection Worker ==
%% Holds rabbitmq connection.
%% Notify subscribers of connection state
%% @end
%%%-------------------------------------------------------------------
-module(rabbit_conn_worker).

-behavior(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/1]).

-export([
    get_connection/0
    ,join_feed/1
    ,leave_feed/1
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([
    init/1
    ,handle_call/3
    ,handle_cast/2
    ,handle_info/2
    ,terminate/2
    ,code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
    connection
    ,params
    ,exchanges
    ,timeout
    ,evtMgr
    ,lastEvent
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({'local', ?SERVER}, ?SERVER, Args, []).

%%--------------------------------------------------------------------
%% @doc
%% Return connection `pid()''
%% @end
%%--------------------------------------------------------------------
-spec get_connection() -> {'ok', pid()}.
get_connection() ->
    gen_server:call(?SERVER, 'get_connection').

%%--------------------------------------------------------------------
%% @doc
%% Join rabbit connection event feed
%% @end
%%--------------------------------------------------------------------
-spec join_feed(pid()) -> 'ok'.
join_feed(Pid) ->
    gen_server:cast(?SERVER, {'join_feed', Pid}).

%%--------------------------------------------------------------------
%% @doc
%% Leave rabbit connection event feed
%% @end
%%--------------------------------------------------------------------
-spec leave_feed({'rabbit_feed', reference()}) -> 'ok'.
leave_feed(ID) ->
    gen_server:cast(?SERVER, {'leave_feed', ID}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(Args) ->
    lager:info("init with: ~p", [Args]),
    process_flag('trap_exit', 'true'),
    Timeout = proplists:get_value('timeout', Args, 5000),
    Username = proplists:get_value('username', Args, "guest"),
    Password = proplists:get_value('password', Args, "guest"),
    Exchanges = proplists:get_value('exchanges', Args, []),
    NetworkParams =
        #amqp_params_network{
            username = rabbit_helper:to_binary(Username)
            ,password = rabbit_helper:to_binary(Password)
            ,host = proplists:get_value('host', Args, "localhost")
            ,port = proplists:get_value('port', Args, 5672)
            ,connection_timeout = Timeout
        },
    {'ok', EvtMgr} = gen_event:start_link({'local', 'rabbit_conn_evt_mgr'}),
    self() ! 'connect',
    {'ok', #state{params=NetworkParams
                  ,exchanges=Exchanges
                  ,timeout=Timeout
                  ,evtMgr=EvtMgr}}.

handle_call('get_connection', _From, #state{connection=Connection}=State) ->
    {'reply', {'ok', Connection}, State};
handle_call(_Msg, _From, State) ->
    lager:warning("rcvd unknown call msg: ~p from: ~p", [_Msg, _From]),
    {'reply', 'ok', State}.

handle_cast({'join_feed', Pid}, #state{evtMgr=EvtMgr, lastEvent=Event}=State) ->
    HandlerId = {'rabbit_feed', make_ref()},
    lager:info("~p joined feed", [Pid]),
    gen_event:add_handler(EvtMgr, HandlerId, [Pid]),
    Pid ! {?MODULE, {'joined', HandlerId}},
    case Event of
        'undefined' -> 'ok';
        _ -> 'ok' = gen_event:notify(EvtMgr, Event)
    end,
    {'noreply', State};
handle_cast({'leave_feed', ID}, #state{evtMgr=EvtMgr}=State) ->
    gen_event:delete_handler(EvtMgr, ID, 'leave_feed'),
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:warning("rcvd unknown cast msg: ~p", [_Msg]),
    {'noreply', State}.

handle_info('connect', #state{params=Params, timeout=Timeout}=State) ->
    Name = erlang:atom_to_binary(node(), 'utf8'),
    lager:info("trying to connect to rabbitmq with ~p", [Params]),
    case amqp_connection:start(Params, Name) of
        {'ok', Connection} ->
            _Ref = erlang:monitor('process', Connection),
            lager:info("connected to rabbitmq"),
            self() ! 'start_exchanges',
            notify({'ok', 'connected'}),
            {'noreply', State#state{connection=Connection}};
        {'error', Reason} ->
            notify({'error', 'connection_failed', Reason}),
            lager:error("rabbitmq connection error ~p, retrying in ~pms", [Reason, Timeout]),
            erlang:send_after(Timeout, self(), 'connect'),
            {'noreply', State}
    end;
handle_info('start_exchanges', #state{connection=Connection, exchanges=Exch}=State) ->
    {'ok', Channel} = amqp_connection:open_channel(Connection),
    'ok' = start_exchanges(Channel, Exch),
    amqp_channel:close(Channel),
    {'noreply', State};
handle_info({'notify', Event}, #state{evtMgr=EvtMgr}=State) ->
    'ok' = gen_event:notify(EvtMgr, Event),
    {'noreply', State#state{lastEvent=Event}};
handle_info({'DOWN', Ref, 'process', Conn, Reason}, #state{connection=Conn}=State) ->
    notify({'error', 'disconnected', Reason}),
    erlang:demonitor(Ref),
    lager:error("rabbitmq connection error ~p", [Reason]),
    self() ! 'connect',
    {'noreply', State#state{connection='undefined'}};
handle_info(_Msg, State) ->
    lager:warning("rcvd unknown info msg: ~p", [_Msg]),
    {'noreply', State}.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

terminate(_Reason, #state{connection='undefined'}) ->
    lager:info("~p terminated: ~p", [?MODULE, _Reason]),
    'ok';
terminate(_Reason, #state{connection=Conn}) ->
    lager:info("~p terminated: ~p", [?MODULE, _Reason]),
    amqp_connection:close(Conn),
    'ok'.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec notify({'ok', 'connected'} | {'error', 'connection_failed' | 'disconnected', _}) -> {'notify', {'ok', 'connected'} | {'error', 'connection_failed' | 'disconnected', _}}.
notify(Event) ->
    self() ! {'notify', Event}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_exchanges(pid(), list()) -> 'ok'.
start_exchanges(_Channel, []) -> 'ok';
start_exchanges(Channel, [Exchange|Exchanges]) ->
    'ok' = start_exchange(Channel, Exchange),
    start_exchanges(Channel, Exchanges).

-spec start_exchange(pid(), list()) -> 'ok'.
start_exchange(Channel, Otps) ->
    Name = proplists:get_value('name', Otps, "amq.topic"),
    Type = proplists:get_value('type', Otps, "topic"),
    Declare =
        #'exchange.declare'{
            exchange = rabbit_helper:to_binary(Name),
            type = rabbit_helper:to_binary(Type),
            durable = 'true'
        },
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare),
    lager:info("declared: ~p exchange ~p", [Type, Name]),
    'ok'.
