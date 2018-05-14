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
-export([start_link/0, zmqsend/1]).

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
-record(state, {socket, state}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

zmqsend(Msg) when is_binary(Msg) ->
    gen_server:cast(?MODULE, {'zmqsend', Msg});
zmqsend(Msg) ->
    lager:warning("will not send ~p", [Msg]).

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
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(_Args) ->
    {'ok', Socket} = ezmq:start_link([{'type', 'dealer'}]),
    self() ! 'connect',
    lager:info("started with ~p",[_Args]),
    {'ok', #state{socket=Socket, state='connecting'}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call(_Request, _From, State) ->
  {'reply', 'ok', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast({'zmqsend', _Msg}, #state{state='connecting'}=State) ->
    lager:debug("dropping message, we are connecting"),
    {'noreply', State};
handle_cast({'zmqsend', Msg}, #state{socket=Socket}=State) ->
    case ezmq:send(Socket, [Msg]) of
        'ok' ->
            _ = dcc_metrics:msg_rate_outgoing(1),
            lager:debug("message sent ~p", [Msg]),
            {'noreply', State};
        _Error ->
            lager:error("error sending msg to zmq ~p, reconnecting", [_Error]),
            self() ! 'connect',
            {'noreply', State#state{state='connecting'}}
    end;
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info('connect', #state{socket=Socket}=State) ->
    {ReqAddr, ReqPort} = get_reqaddr_port(),
    case ezmq:connect(Socket, 'tcp', ReqAddr, ReqPort, [{'timeout', 5000}]) of
        'ok' ->
            Msg = msgpack:pack(#{"name" => "noop", "nodeid" => "DCC"}),
            case ezmq:send(Socket, [Msg]) of
                'ok' ->
                    lager:info("zmq connection ready"),
                    {'noreply', State#state{state='connected'}};
                _Error ->
                    lager:error("error checking connection: ~p, reconnecting", [_Error]),
                    erlang:send_after(5000, self(), 'connect'),
                    {'noreply', State#state{state='connecting'}}
            end;
        _Error ->
            lager:error("error creating connection: ~p, reconnecting", [_Error]),
            erlang:send_after(5000, self(), 'connect'),
            {'noreply', State#state{state='connecting'}}
    end;
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_reqaddr_port() ->
    {'ok', HaproxyAddr} = application:get_env('haproxy'),
    %{ok, HaproxyAddr} = inet_parse:address(HaproxyA1),
    {'ok', ReqA1} = application:get_env('device_service', 'reqaddr'),
    [_, ReqA2] = string:tokens(ReqA1, "//"),
    [_, PortStr] = string:tokens(ReqA2,":"),
    ReqPort = list_to_integer(PortStr),
    lager:debug("zmqpushclient addr: ~p, port: ~p",[HaproxyAddr, ReqPort]),
    {HaproxyAddr, ReqPort}.
