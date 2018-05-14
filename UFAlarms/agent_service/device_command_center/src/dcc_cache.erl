-module(dcc_cache).

-behavior(gen_server).

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    start_link/0
    ,insert/2
    ,lookup/1
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
-define(TABLE, ?MODULE).

-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?SERVER, [], []).

insert(Key, Data) ->
    _ = dcc_metrics:cache_rate_insert(1),
    gen_server:cast(?SERVER, {'insert', Key, Data}).

lookup(Key) ->
    _ = dcc_metrics:cache_rate_read(1),
    dcc_metrics:cache_timer_read(
        'gen_server',
        'call',
        [?SERVER, {'lookup', Key}]
    ).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(_Args) ->
    _ = ets:new(?TABLE, ['set', 'named_table', 'public']),
    lager:info("started ~p", [?SERVER]),
    {'ok', #state{}}.

handle_call({'lookup', Key}, _From, State) ->
    lager:debug("rcvd lookup request from ~p for ~p", [_From, Key]),
    Reply =
        case ets:lookup(?TABLE, Key) of
            [] ->
                lager:debug("found nothing for key ~p", [Key]),
                'undefined';
            [{Key, Data}|_] -> Data
        end,
    {'reply', Reply, State};
handle_call(_Msg, _From, State) ->
    lager:debug("rcvd unknown call msg: ~p from: ~p", [_Msg, _From]),
    {'reply', 'ok', State}.

handle_cast({'insert', Key, Data}, State) ->
    lager:debug("inserting ~p: ~p", [Key, Data]),
    _ = ets:insert(?TABLE, {Key, Data}),
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("rcvd unknown cast msg: ~p", [_Msg]),
    {'noreply', State}.

handle_info(_Msg, State) ->
    lager:debug("rcvd unknown info msg: ~p", [_Msg]),
    {'noreply', State}.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

terminate(_Reason, _State) ->
    lager:error("~p terminated: ~p", [?MODULE, _Reason]),
    'ok'.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
