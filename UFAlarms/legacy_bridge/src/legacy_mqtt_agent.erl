-module(legacy_mqtt_agent).

-author("Gina Hagg ghagg@sensity.com").

-behaviour(gen_server).
-behaviour(poolboy_worker).

-compile([{parse_transform, lager_transform}]).

-define(MOSCA_WILL_TOPIC, setwilltopic()).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, start_link/1, stop/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([code_change/3, handle_call/3, handle_cast/2,
     handle_info/2, init/1, terminate/2]).

-record(state, {mqttc=[], subscribed=[]}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
stop() ->
    lager:info("stop is called"),
    gen_server:call(?MODULE, stop).

start_link(Args) ->
    lager:info("~p is starting",[?MODULE]),
    gen_server:start_link(?MODULE, Args, []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
              []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(_Args) ->
    self() ! 'post_init',
    {'ok', #state{mqttc = 'undefined'}}.

%%calls
handle_call({'subscribe', Topic}, _From, State = #state{mqttc = Con, subscribed = Subscribed} ) ->
    lager:info("subscribing ~p..",[?MODULE]),
    case lists:member(Topic,Subscribed) of 'true' ->lager:debug("already registered for Topic:~p",[Topic]),
             {'reply', Con, State};
        _-> emqttc:subscribe(Con, Topic, 'qos0'),
            {'reply', Con, State#state{subscribed = [Topic|Subscribed]}}
        end;
handle_call('stop', _From, State) ->
    lager:info("stopping ~p..", [?MODULE]),
    {'stop', 'normal', 'ok', State};
handle_call(_Request, _From, State) ->
    {'noreply', State}.


%%casts
handle_cast({'publish_to_mqtt', Msg, Topic}, State=#state{mqttc=Con}) ->
    lager:debug("sending msg: ~p, to topic: ~p[] with Con: ~p",[Msg, Topic, Con]),
    emqttc:publish(Con, Topic, Msg, 0),
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%infos
handle_info({'publish', Topic, _Payload}, State) ->
    lager:debug("received payload with topic ~p",[Topic]),
    %Tokens = string:tokens(binary_to_list(Topic), "/"),
    %[_,Orgid,Siteid,Nodeid,_] = Tokens,
    %lager:debug("Orgid: ~p, Siteid:~p, Nodeid:~p",[Orgid, Siteid,Nodeid]),
    {'noreply', State};
%% Client connected
handle_info({'mqttc', Con, 'connected'}, State) ->
    lager:info("~p: YAY is connected with con: ~p.", [?MODULE, Con]),
    case gproc:where({'n', 'l', 'mqttc'}) of
        'undefined' -> gproc:reg({'n', 'l', 'mqttc'},Con);
        _-> gproc:unreg({'n', 'l', 'mqttc'}), gproc:reg({'n', 'l', 'mqttc'},Con)
    end,
    {'noreply', State};

%% Client disconnected
handle_info({'mqttc', C,  'disconnected'}, State = #state{mqttc = C}) ->
    lager:info("~p: with conn: ~p is disconnected", [?MODULE, C]),
    try gproc:unreg({'n', 'l', 'mqttc'}) catch
        'error':'badarg' ->
            {lager:debug("MQTTC_DISCONNECT!!Couldn't unregister gproc for mqttc ~p", [C])}
    end,
    {noreply, State};

handle_info(post_init, State) ->
    case connect_to_mqtt(?MODULE) of
    {'ok', C} ->
        lager:info("GINA--emqttc connected in reconnect with C:~p",[C]),
        try gproc:reg({'n', 'l', 'mqttc'},C) catch
            'error':'badarg' -> {lager:debug("MTTTCC_CONNECT!!Couldn't register gproc for mqttc ~p", [C])}
        end,
        {'noreply', #state{mqttc = C}};
    _ ->
        lager:info("no connection, will try again in reconnect"),
        {'noreply', State}
    end;
handle_info({'shutdown', 'tcp_closed'}, State) ->
    lager:debug("info shutdown"), {'noreply', State};

handle_info(Msg, State) ->
    lager:debug("received unexpected msg ~p",[Msg]),
    {'noreply', State}.

terminate({'shutdown', 'tcp_closed'}, State) ->
    lager:info("terminate, shutdown"), {'ok', State};
terminate(Reason, _State) ->
    lager:info("terminate, reason ~p", [Reason]),
    'ok'.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% private
%% ------------------------------------------------------------------
connect_to_mqtt(ClientId) ->
    {'ok', Host} = application:get_env('legacy_bridge', 'moscahost'),
    {'ok', Port} = application:get_env('legacy_bridge', 'moscaport'),
    {'ok', Reconnect} = application:get_env('legacy_bridge', 'mqttreconnect'),
    {'ok', Loggerlevel} = application:get_env('legacy_bridge', 'mqqtloglevel'),
    {'ok', KeepAlive} = application:get_env('legacy_bridge', 'keepalive'),
    Will = [{'qos', 1}, {'retain', 'false'}, {'topic', ?MOSCA_WILL_TOPIC}, {'payload', <<"I die">>}],
    emqttc:start_link([
            'auto_resub'
            ,{'host',Host}
            ,{'port', Port}
            ,{'keepalive', KeepAlive}
            ,{'client_id', ClientId}
            ,{'reconnect', Reconnect}
            ,{'will', Will}
            ,{'logger', {'lager', Loggerlevel}}
        ]).

setwilltopic() ->
  {'ok', Host} = inet:gethostname(),
  list_to_binary("/streamv1/legacy/" ++ Host ++ "/will").
