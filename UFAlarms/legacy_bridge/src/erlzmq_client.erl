-module(erlzmq_client).
-author("Gina Hagg ghagg@sensity.com").

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).
%Pid = spawn(fun () -> any_function(Any, Number, Of, Arguments) end)

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, stop/0, zmqsend/1]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%-record(nodes,{nodeid = [], orgid = [], siteid=[]}).
-record(state, {ctx=[], ct=0, socket=[], connret = []}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

zmqsend(Msg) ->
    lager:info("zmqsend is called with msg ~p~n",[Msg]),
    gen_server:cast(?MODULE, {zmqsend, Msg}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    pg2:join(zmq_pp_group, self()),
    {ok, Ctx} = erlzmq:context(),
    SockRet  = erlzmq:socket(Ctx, [req, {active, false}]),
    State = case SockRet of 
        {ok, Reqsocket} ->
            lager:info("zmqclient: successfully created Reqsocket ~p~n", [Reqsocket]),
            erlzmq:setsockopt(Reqsocket, linger, 0),
            %erlzmq:setsockopt(Reqsocket, rcvtimeo, 0),
            {ok,ReqA1} = application:get_env(reqaddr),
            [_, ReqA2] = string:tokens(ReqA1, "//"),
            [Host, _] = string:tokens(ReqA2,":"),
            NewAddr = Host ++ ":" ++ "6537",
            ConnRet = erlzmq:connect(Reqsocket, NewAddr),
            lager:debug("Connret is ~p~n",[ConnRet]),
            #state{ctx = Ctx, socket = Reqsocket, connret = ConnRet};
        BadSock -> lager:warning("create Socket returned ~p, in case of {error,eterm} ,context closed before some blocking sockets closed.  ignore it",[BadSock]),
        #state{ctx = Ctx, socket = []}
    end,
    {ok, State}.

%% ErlZMQClient got send request from DCC
handle_cast({zmqsend, Msg}, State = #state{socket = Socket}) ->  
    zmq_send(Msg, Socket),
    {noreply,State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{ctx=Ctx}) ->
    lager:info("erlzmqClient=>terminate called commandcenter: ~p~n",[Reason]),
    TermRet =erlzmq:term(Ctx),
    lager:info("client: erlzmq terminate ctx returned ~p~n", [TermRet]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% ezmq client Function Definitions
%% ------------------------------------------------------------------

zmq_send(Msg, Socket) ->
    case Socket of [] -> 
        lager:warning("no socket, CANT SEND MSG"); 
        _->
            send_if_connected(Socket,Msg)
    end.

send_if_connected(Reqsocket, Msg) ->
    lager:info("zmqclient: successfully connected with Reqsocket ~p~n", [Reqsocket]),
    Zret = erlzmq:send(Reqsocket, Msg,[dontwait]),
    lager:info("client: normal erlzmq SEND returned ~p for socket ~p~n", [Zret, Reqsocket]),
    receive_response(Reqsocket).

close_socket(Reqsocket) ->
    CloseRet = erlzmq:close(Reqsocket),
    case CloseRet of 
        ok ->
            lager:info("client:close_socket:normal,socket:~p closed and released thread. Good!!~n", [Reqsocket]);
        {error,eterm} ->
            lager:info("client:erlzmq CLOSE returned eterm error for socket ~p. harmless. no worries, context terminate will close it~n",[Reqsocket]);
        {error,Another} -> 
            lager:info("client:error erlzmq CLOSE for socket ~p returned error. ~p~n.",[Another,Reqsocket])
    end.   

receive_response(Reqsocket) ->
    Recv = erlzmq:recv(Reqsocket),
    case Recv of
        {ok,_Reply} ->
            %M = msgpack:unpack(Reply,[{format,map}]),
            lager:info("Unpacked reply for socket ~p is ok~n", [Reqsocket]),
            close_socket(Reqsocket);
        {error,eagain} ->
            lager:info("recieve_response:client RECV {error,eagain} for socket ~p, there was no message to pick up on the datadealer side, we are non-blocking. just ignore",[Reqsocket]),
            close_socket(Reqsocket);
        {error,eterm} ->
            lager:info("recieve_response:client RECV {error,eterm} for socket ~p, means, context closed and receive still blocking. Datadealer didn't respond within 1s",[Reqsocket]),
            close_socket(Reqsocket);
        Other -> lager:info("Received an error msg at zmq:receive for socket:~p, ~p~n",[Reqsocket,Other]),  close_socket(Reqsocket)
    end.