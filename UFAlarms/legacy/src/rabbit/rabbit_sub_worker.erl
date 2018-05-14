%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == RabbitMQ Subscriber Worker ==
%% Create a channel and queue (bind it).
%% Listen for messages and transfer them to `legacy_cmd_handler'
%% @end
%%%-------------------------------------------------------------------
-module(rabbit_sub_worker).

-include_lib("amqp_client/include/amqp_client.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/1]).

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

-record(state, {channel, queue, tag, msg_handler}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?SERVER, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(Args) ->
    {'ok', Conn} = rabbit_conn_worker:get_connection(),
    Exchange = rabbit_helper:to_binary(proplists:get_value('exchange', Args)),
    Key = rabbit_helper:to_binary(proplists:get_value('key', Args)),
    QName = rabbit_helper:to_binary(proplists:get_value('queue', Args)),
    MsgHanlder = proplists:get_value('msg_handler', Args),

    {'ok', Channel} = amqp_connection:open_channel(Conn),

    QDeclare = #'queue.declare'{
        auto_delete = true
        ,queue = QName
    },

    #'queue.declare_ok'{queue=Q} = amqp_channel:call(Channel, QDeclare),
    lager:info("queue ~p created with ~p", [QName, QDeclare]),

    Binding = #'queue.bind'{queue=Q,
                            exchange=Exchange,
                            routing_key=Key},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
    lager:info("queue bound to ~p with ~p", [Exchange, Key]),

    Sub = #'basic.consume'{queue=Q},
    #'basic.consume_ok'{consumer_tag=Tag} = amqp_channel:call(Channel, Sub),
    lager:info("subscribed ~p", [Tag]),

    {'ok', #state{channel=Channel, queue=Q, tag=Tag, msg_handler=MsgHanlder}}.

handle_call(_Msg, _From, State) ->
    lager:debug("rcvd unknown call msg: ~p from: ~p", [_Msg, _From]),
    {'reply', 'ok', State}.

handle_cast(_Msg, State) ->
    lager:debug("rcvd unknown cast msg: ~p", [_Msg]),
    {'noreply', State}.

handle_info({#'basic.deliver'{delivery_tag=Tag, routing_key=Key}
            ,{'amqp_msg', #'P_basic'{}, Content}}
            ,#state{channel=Channel, msg_handler=MsgHanlder}=State) ->
    lager:debug("rcvd msg with key: ~p, sending to ~p:handle/1", [Key, MsgHanlder]),
    spawn(MsgHanlder, 'handle', [Content]),
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag=Tag}),
    {'noreply', State};
handle_info(#'basic.consume_ok'{consumer_tag=Tag}, #state{tag=Tag}=State) ->
    lager:info("worker sucessfully subscribed to queue with: ~p", [Tag]),
    {'noreply', State};
handle_info(_Msg, State) ->
    lager:debug("rcvd unknown info msg: ~p", [_Msg]),
    {'noreply', State}.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

terminate(_Reason, #state{channel=Channel}) ->
    lager:warning("~p terminated: ~p", [?MODULE, _Reason]),
    amqp_channel:close(Channel),
    'ok'.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
