-module(zmqclientdealer).
-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, stop/1]).
-export([start_link/0, zmqsend/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(HI,<<"§HiBrian">>).
-define(OKMSGBIN, <<129,196,6,115,116,97,116,117,115,196,12,103,105,110,97,45,115,117,99,99,101,115,115>>).
-record(state, {ctx=[], socket= []}).
-define(TICK, 10). % 10millsecs

%% API implementation
start_link() ->
    lager:info("~p: zmqclientdealer is starting",[?MODULE]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(ConnectArgs) ->
    gen_server:start_link(?MODULE, {init, ConnectArgs}, []).

start_link(RegisterName, ConnectArgs) ->
    gen_server:start_link({local, RegisterName}, ?MODULE, {init, ConnectArgs}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

zmqsend(Msg) ->
    lager:info("zmqsend is called with msg ~p~n",[Msg]),
    gen_server:cast(?MODULE, {zmqsend, Msg}).

%% gen_server implementation
%init({init, ConnectArgs}) ->
%    {ok, Socket} = ezmq:start([{type, dealer}]),
%    ok = erlang:apply(fun ezmq:connect/5, [Socket|ConnectArgs]),
%    spawn_link(fun() -> recv_loop(Socket) end),
%    {ok, {socket, Socket}}.

init(_Args) ->
    {ok, Socket} = ezmq:start([{type, req}]),
    {ReqAddr, ReqPort} = get_reqaddr_port(),
    %ok = erlang:apply(fun ezmq:connect/5, [Socket|ConnectArgs]),
    ezmq:connect(Socket, tcp, ReqAddr, ReqPort, []),
    exometer:new([zmqsends], counter),
    exometer:new([zmqreplies], counter),
    exometer:new([clientins],  counter),
    exometer_report:add_reporter(
         exometer_report_lager,[{type_map,[{'_',integer}]}]),
    exometer_report:subscribe(exometer_report_lager,[clientins],[value],15000),
    exometer_report:subscribe(exometer_report_lager,[zmqsends],[value],15000),
    exometer_report:subscribe(exometer_report_lager,[zmqreplies],[value],15000),
    %lager:debug("sending HI"),
    %recv_loop(Socket),
    {ok, #state{socket=Socket}}.

handle_info({zmqsend, Msg}, State = #state{socket = Socket}) ->  
    lager:debug("next is recv_loop in info"),
    spawn_link(fun() -> recv_loop(Socket, os:system_time(milli_seconds),[<<>>, Msg]) end),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

handle_call({zmqsend, Msg}, _From, State = #state{socket=Socket}) -> 
    %FromBin = erlang:term_to_binary(From),
    spawn_link(fun() -> recv_loop(Socket, os:system_time(milli_seconds),[<<>>, Msg]) end),
    {noreply,State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%% EZMQClient got send request from DCC
handle_cast({zmqsend, Msg}, State = #state{socket = Socket}) ->
    lager:debug("next is recv_loop in cast"),
    exometer:update([clientins], 1),
    spawn_link(fun() -> recv_loop(Socket, os:system_time(milli_seconds),Msg) end),
    {noreply,State};
handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, {socket, Socket}) ->
    ezmq:close(Socket);
terminate(_Reason, _State) ->
    ok.

recv_loop(Socket) ->
    lager:debug("Recv_loop~n"),
    {ok, [<<>>, ReplyBin]} = ezmq:recv(Socket),
    lager:debug("Recv_loop received: ~p~n",[ReplyBin]),
    recv_loop(Socket).

recv_loop(Socket, Timestamp,Msg) ->
    %lager:debug("Recv_loop with timestamp: ~p~n",[Timestamp]),
    %{ok, [<<>>, ReplyBin]} = ezmq:recv(Socket),
    D = ezmq:recv(Socket),
    exometer:update([zmqreplies], 1),
    lager:debug("Recv_loop received: ~p~n",[D]),
    Now = os:system_time(milli_seconds),
    case (Now-Timestamp) < ?TICK of true ->
        recv_loop(Socket, Timestamp, Msg);
    _->
       Res = ezmq:send(Socket, [<<>>,Msg]),
       lager:debug("sent message, exiting recv_loop with Res ~p~n",[Res]),
       exometer:update([zmqsends], 1)
    end.

get_reqaddr_port() ->
{ok,ReqA1} = application:get_env(reqaddr),
    [_, ReqA2] = string:tokens(ReqA1, "//"),
    [Host, PortStr] = string:tokens(ReqA2,":"),
    ReqPort = list_to_integer(PortStr),
    {ok,ReqAddr} = inet_parse:address(Host),
    lager:info("legacy_zmq_client:zmq req addr:~p, port:~p",[ReqAddr,ReqPort]),
{ReqAddr,ReqPort}.

