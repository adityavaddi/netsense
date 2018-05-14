-module(legacy_zmq_client).
-author("Gina Hagg ghagg@sensity.com").

-behaviour(gen_server).
-behaviour(poolboy_worker).

-compile([{parse_transform, lager_transform}]).
-define(IDLE_TIMEOUT, 10000).
%Pid = spawn(fun () -> any_function(Any, Number, Of, Arguments) end)

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, start_link/0, stop/0, zmqsend/1]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%-record(nodes,{nodeid = [], orgid = [], siteid=[]}).
-record(state, {ctx=[], socket= []}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    lager:debug("EZMQCLient: legacy_zmq_client is starting"),
    %gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    gen_server:start_link(?MODULE, Args, []).

start_link() ->
    lager:debug("EZMQCLient: legacy_zmq_client is starting"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
   
stop() ->
    gen_server:call(?MODULE, stop).

zmqsend(Msg) ->
    lager:debug("ezmqsend is called with msg ~p~n",[Msg]),
    gen_server:cast(?MODULE, {zmqsend, Msg}).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    pg2:join(zmq_other_group, self()),   
    {ok, Socket} = ezmq:start([{type, req},{active, false}]),    
    {ReqAddr, ReqPort} = get_reqaddr_port(),
    ezmq:connect(Socket, tcp, ReqAddr, ReqPort, []),
    {ok, #state{socket=Socket}}.

%% ErlZMQClient got send request from DCC
handle_cast({zmqsend, Msg}, State = #state{socket = Socket}) ->  
    send_msg(Socket, Msg),
    {noreply,State, ?IDLE_TIMEOUT};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({zmqsend, Msg}, _From, State = #state{socket = Socket}) ->  
    send_msg(Socket, Msg), 
    {noreply,State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info({zmqsend, Msg}, State = #state{socket = Socket}) ->  
    spawn(fun() -> send_msg(Socket, Msg) end), 
    {noreply,State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{socket=Socket}) ->
    lager:debug("ezmqClient=>terminate called legacy_zmq_client: ~p~n",[Reason]),
    close_socket(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% ezmq client Function Definitions
%% ------------------------------------------------------------------


send_msg(Reqsocket, Msg) ->
    %lager:debug("ezmqclient: successfully connected with Reqsocket ~p~n", [Reqsocket]),
    Zret = ezmq:send(Reqsocket, [Msg]),
    lager:debug("ezmqclient: normal erlzmq SEND returned ~p for socket ~p~n", [Zret, Reqsocket]),
    %exometer:update(zmqsends, 1),
    receive_response(Reqsocket).

close_socket(Reqsocket) ->
    CloseRet = ezmq:close(Reqsocket),
    case CloseRet of 
        ok ->
            exometer:update(zmqreplies, 1),
            lager:debug("ezmqclient:close_socket:normal,socket:~p closed and released thread. Good!!~n", [Reqsocket]);
        {error,eterm} ->
            lager:debug("ezmqclient:ezmq CLOSE returned eterm error for socket ~p. harmless. no worries, context terminate will close it~n",[Reqsocket]);
        {error,Another} -> 
            lager:debug("ezmqclient:error erlzmq CLOSE for socket ~p returned error. ~p~n.",[Another,Reqsocket])
    end.   

receive_response(Reqsocket) ->
    Recv = ezmq:recv(Reqsocket),
    case Recv of
        {ok,_} ->
            %exometer:update(zmqreplies, 1),
            lager:debug("Unpacked reply for socket ~p is ok~n", [Reqsocket]);
        {error,eagain} ->
            lager:debug("recieve_response:client RECV {error,eagain} for socket ~p, there was no message to pick up on the datadealer side, we are non-blocking. just ignore",[Reqsocket]);
        {error,eterm} ->
            lager:debug("recieve_response:client RECV {error,eterm} for socket ~p, means, context closed and receive still blocking. Datadealer didn't respond within 1s",[Reqsocket]);
        Other -> lager:debug("Received an error msg at zmq:receive for socket:~p, ~p~n",[Reqsocket,Other])
    end.

get_reqaddr_port() ->
{ok,ReqA1} = application:get_env(legacy_bridge,reqaddr),
    [_, ReqA2] = string:tokens(ReqA1, "//"),
    [Host, PortStr] = string:tokens(ReqA2,":"),
    ReqPort = list_to_integer(PortStr),
    {ok,ReqAddr} = inet_parse:address(Host),
    lager:debug("legacy_zmq_client:zmq req addr:~p, port:~p",[ReqAddr,ReqPort]),
{ReqAddr,ReqPort}.