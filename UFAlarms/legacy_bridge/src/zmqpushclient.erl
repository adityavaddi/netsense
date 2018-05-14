%%%-------------------------------------------------------------------
%%% @author brefsdal
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Oct 2016 8:44 AM
%%%-------------------------------------------------------------------
-module(zmqpushclient).
-author("brefsdal").

-behaviour(gen_server).

%% API
-export([
    start_link/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-compile([{parse_transform, lager_transform}]).

-define(SERVER, ?MODULE).
-record(state, {socket}).
-define(IDLE_TIMEOUT, 10000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
    lager:info("~p: is starting",[?MODULE]),
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {'ok', Socket} = ezmq:start([{'type', 'dealer'}]),
    {ReqAddr, ReqPort} = get_reqaddr_port(),
    'ok' = ezmq:connect(Socket, 'tcp', ReqAddr, ReqPort, [{'timeout', 'infinity'}]),
    {'ok', #state{socket=Socket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({'zmqsend', Msg}, _From, #state{socket=Socket}=State) ->
    Reply = send_msg(Socket, Msg),
    {'reply', Reply, State};
handle_call('stop', _From, State) ->
    {'stop', 'normal', 'ok', State};
handle_call(_Request, _From, State) ->
    {'reply', 'ok', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({'zmqsend', Msg}, #state{socket=Socket}=State) ->
    _ = send_msg(Socket, Msg),
    {'noreply', State, ?IDLE_TIMEOUT};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket=Socket}) ->
    ezmq:close(Socket),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_msg(Reqsocket, Msg) ->
    case ezmq:send(Reqsocket, [Msg]) of
        'ok' -> 'ok';
        {'error', Reason}=Error ->
            lager:error("failed to send msg ~p to ~p with error ~p", [Msg, Reqsocket, Reason]),
            Error
    end.

get_reqaddr_port() ->
    {'ok', HaproxyAddr} = application:get_env('legacy_bridge', 'haproxy'),
    %{'ok', HaproxyAddr} = inet_parse:address(HaproxyA1),
    {'ok',ReqA1} = application:get_env('legacy_bridge', 'reqaddr'),
    [_, ReqA2] = string:tokens(ReqA1, "//"),
    [_, PortStr] = string:tokens(ReqA2, ":"),
    ReqPort = list_to_integer(PortStr),
    lager:debug("zmqpushclient addr: ~p, port: ~p", [HaproxyAddr, ReqPort]),
    {HaproxyAddr, ReqPort}.
