%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == RabbitMQ Connection Watcher ==
%% Starts Subscriber and Load Balancer when connection is ready
%% @end
%%%-------------------------------------------------------------------
-module(rabbit_conn_watcher).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    start_link/1
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
    feed
    ,connected
    ,pids
    ,args
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({'local', ?SERVER}, ?SERVER, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(Args) ->
    'ok' = rabbit_conn_worker:join_feed(self()),
    lager:info("started ~p with ~p", [?SERVER, Args]),
    {'ok', #state{connected='false', pids=[], args=Args}}.

handle_call(_Msg, _From, State) ->
    lager:warning("rcvd unknown call msg: ~p from: ~p", [_Msg, _From]),
    {'reply', 'ok', State}.

handle_cast(_Msg, State) ->
    lager:warning("rcvd unknown cast msg: ~p", [_Msg]),
    {'noreply', State}.

handle_info({'rabbit_conn_worker', {'joined', FeedID}}, State) ->
    lager:info("subscribed to rabbit_conn_worker"),
    {'noreply', State#state{feed=FeedID}};
handle_info({'rabbit_feed', {'ok', 'connected'}}, #state{connected='false', args=Args}=State) ->
    lager:info("rabbit connection connected"),
    LBArgs = proplists:get_value('publisher', Args, []) ++ [{'worker', 'rabbit_pub_worker'}, {'restart', 'true'}],

    {'ok', LBPid} = elb:start_link(LBArgs),
    'true' = erlang:register('rabbit_load_balancer', LBPid),

    SubArgs = proplists:get_value('subscriber', Args, []),
    {'ok', SubPid} = gen_server:start_link('rabbit_sub_worker', SubArgs, []),
    {'noreply', State#state{connected='true', pids=[LBPid, SubPid]}};
handle_info({'rabbit_feed', {'ok', 'connected'}}, State) ->
    lager:debug("already connected"),
    {'noreply', State};
handle_info({'rabbit_feed', {'error', Status, Reason}}, #state{pids=Pids}=State) ->
    lager:error("rabbit connection: ~p ~p", [Status, Reason]),
    lists:foreach(
        fun(Pid) ->
            gen_server:stop(Pid, 'rabbit_conn_down', 1000)
        end
        ,Pids
    ),
    {'noreply', State#state{connected='false'}};
handle_info(_Msg, State) ->
    lager:warning("rcvd unknown info msg: ~p", [_Msg]),
    {'noreply', State}.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

terminate(_Reason, _State) ->
    lager:error("~p terminated: ~p", [?MODULE, _Reason]),
    'ok'.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
