-module(zmqpulllistener).

-behavior(gen_server).

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).

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
-define(OKMSGBIN, <<129,196,6,115,116,97,116,117,115,196,12,103,105,110,97,45,115,117,99,99,101,115,115>>).
-record(state, {socket}).
-define(DISTRIBUTOR_GROUP, 'distributor_group').

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?SERVER, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(_Args) ->
    {'ok', Socket} = ezmq:start([{'type', 'router'}, {'active', 'true'}]),
    ReqPort = get_repaddr_port(),
    'ok' = ezmq:bind(Socket, 'tcp', ReqPort, [{'reuseaddr', 'true'}]),
    lager:info("zmqpulllistener started on port ~p", [ReqPort]),
    {ok, #state{socket=Socket}}.


handle_call(_Msg, _From, State) ->
    lager:debug("rcvd unknown call msg: ~p from: ~p", [_Msg, _From]),
    {'reply', 'ok', State}.

handle_cast(_Msg, State) ->
    lager:debug("rcvd unknown cast msg: ~p", [_Msg]),
    {'noreply', State}.

handle_info({'zmq', Pid, {Address, [Msg]}}, #state{socket=Socket}=State) ->
    Reply = ezmq:send(Socket, {Address, [?OKMSGBIN]}),
    lager:debug("rcvd ~p with resp ~p", [Msg, Reply]),
    Which = helpers:get_best_pid(?DISTRIBUTOR_GROUP),
    Which ! {'command', Msg},
    {'noreply', State};
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_repaddr_port() -> integer().
get_repaddr_port() ->
    {'ok', ReqA1} = application:get_env('legacy_bridge', zmqdealer),
    [_, ReqA2] = string:tokens(ReqA1, "//"),
    [_, PortStr] = string:tokens(ReqA2,":"),
    ReqPort = list_to_integer(PortStr),
    ReqPort.
