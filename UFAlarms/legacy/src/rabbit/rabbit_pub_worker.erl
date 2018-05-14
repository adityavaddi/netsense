%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == RabbitMQ Publisher Worker ==
%% Create a channel and publish message threw it
%% @end
%%%-------------------------------------------------------------------
-module(rabbit_pub_worker).

-include_lib("amqp_client/include/amqp_client.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, start_link/1]).

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

-record(state, {channel}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    ?MODULE:start_link([]).

start_link(Args) ->
    gen_server:start_link(?SERVER, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(_Args) ->
    process_flag('trap_exit', 'true'),
    {'ok', Conn} = rabbit_conn_worker:get_connection(),
    {'ok', Channel} = amqp_connection:open_channel(Conn),
    'ok' = amqp_channel:register_flow_handler(Channel, self()),
    lager:debug("started ~p", [?SERVER]),
    {'ok', #state{channel=Channel}}.

handle_call(_Msg, _From, State) ->
    lager:debug("rcvd unknown call msg: ~p from: ~p", [_Msg, _From]),
    {'reply', 'ok', State}.

handle_cast(_Msg, State) ->
    lager:debug("rcvd unknown cast msg: ~p", [_Msg]),
    {'noreply', State}.

handle_info({{'publish', Exchange, Key, Payload}, From}, #state{channel=Channel}=State) ->
    lager:debug("publishing to channel: ~p, exchange: ~p, key: ~p", [Channel, Exchange, Key]),
    Publish = #'basic.publish'{exchange=Exchange, routing_key=Key},
    Content = #amqp_msg{payload=Payload},
    Reply = amqp_channel:call(Channel, Publish, Content),
    elb:reply(From, Reply),
    {'noreply', State};
handle_info(#'channel.flow'{active=Active}, State) ->
    lager:warning("rcvd a flow control message ~p", [Active]),
    {'noreply', State};
handle_info(_Msg, State) ->
    lager:warning("rcvd unknown info msg: ~p", [_Msg]),
    {'noreply', State}.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

terminate(_Reason, #state{channel=Channel}) ->
    lager:error("~p terminated: ~p", [?MODULE, _Reason]),
    amqp_channel:close(Channel),
    'ok'.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
