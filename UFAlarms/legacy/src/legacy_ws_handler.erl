%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Legacy Websocket handler ==
%% Websocket hanlder for nodes
%% @end
%%%-------------------------------------------------------------------
-module(legacy_ws_handler).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    init/2
    ,websocket_init/1
    ,websocket_handle/2
    ,websocket_info/2
    ,terminate/3
]).

-include("legacy.hrl").

-record(state, {nodeid, timer, keepalive}).

-type state() :: #state{}.
-type req() :: cowboy_req:req().
-type reply() :: {'ok', _} | {'stop', _} | {'reply', [{_ ,_}] | {'binary', <<_:48>>}, state()}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(req(), list()) -> {'cowboy_websocket', req(), state()}.
init(Req, _Opts) ->
    NodeID = cowboy_req:binding('node_id', Req),
    legacy_metrics:connections_count(1),
    {'cowboy_websocket', Req, #state{nodeid=NodeID}}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec websocket_init(state()) -> {'ok', state()}.
websocket_init(#state{nodeid=NodeID}=State) ->
    legacy_helpers:init_lager(NodeID),
    {'ok', Keepalive} = legacy_config:get('keepalive'),
    self() ! 'register',
    Timer = erlang:send_after(Keepalive, self(), 'keepalive'),
    lager:info("node ~p connected", [NodeID]),
    {'ok', State#state{timer=Timer, keepalive=Keepalive}}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec websocket_handle(any(), state()) -> any().
websocket_handle({'binary', Msg}, #state{nodeid=NodeID}=State) ->
    lager:debug("~p got a message ~p", [NodeID, Msg]),
    legacy_metrics:msg_rate_incoming(1),
    Self = self(),
    spawn(
        fun() ->
            legacy_metrics:msg_timer(
                'legacy_msg_handler'
                ,'handle'
                ,[NodeID, Msg, Self]
            )
        end
    ),
    {'ok', State};
websocket_handle(_Msg, State) ->
    lager:debug("got unknown message ~p", [_Msg]),
    {'ok', State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec websocket_info(any(), state()) -> reply().
websocket_info({'command', Payload}, #state{nodeid=NodeID}=State) when is_binary(Payload) ->
    legacy_metrics:cmd_rate_outgoing(1),
    lager:debug("web socket rcv command: ~p sending to node: ~p", [Payload, NodeID]),
    Reply = [{'binary', Payload}],
    {'reply', Reply, State};
websocket_info({'command', Payload}, #state{nodeid=NodeID}=State) when is_list(Payload) ->
    lager:debug("web socket rcv commands: ~p sending to node: ~p", [Payload, NodeID]),
    legacy_metrics:cmd_rate_outgoing(erlang:length(Payload)),
    Reply = [{'binary', Bin} || Bin <- Payload],
    {'reply', Reply, State};
websocket_info('register', #state{nodeid=NodeID}=State) ->
    lager:debug("received a register request for ~p", [NodeID]),
    case register_node(NodeID) of
        'ok' ->
            lager:info("registration successful, sending connected message"),
            erlang:spawn('legacy_msg_handler', 'connected_msg', [NodeID]),
            {'ok', State};
        'error' -> {'stop', State};
        {'retire', Pid} ->
            Self = self(),
            lager:debug("transfering ~p, from ~p to ~p  and killing ~p", [NodeID, Pid, Self, Pid]),
            _ = erlang:monitor('process', Pid),
            Pid ! {'retire', Self},
            {'ok', State}
    end;
websocket_info('keepalive', #state{timer=OldTimer, nodeid=NodeID, keepalive=Keepalive}=State) ->
    legacy_metrics:cmd_rate_outgoing(1),
    erlang:cancel_timer(OldTimer),
    Timer = erlang:send_after(Keepalive, self(), 'keepalive'),
    Reply = {binary, ?BOGUS_SENSOR_PAYLOAD},
    lager:debug("sending bogus message to check keepalive for ~p", [NodeID]),
    {'reply', Reply, State#state{timer=Timer}};
websocket_info({'retire', To}, #state{nodeid=NodeID}=State) ->
    lager:info("received a retire request from ~p, shuting down", [To]),
    gproc:give_away({'n', 'g', legacy_helpers:node_key(NodeID)}, To),
    {'stop', State};
websocket_info({'DOWN', Ref, 'process', _Pid, _Reason}, #state{nodeid=NodeID}=State) ->
    Self = self(),
    lager:debug("received a DOWN from ~p : ~p", [_Pid, _Reason]),
    _ = erlang:demonitor(Ref),
    lager:debug("demonitoring from ~p and checking registration", [_Pid]),
    % This is to make sure that the node is registered to the right process
    case gproc:lookup_global_name(legacy_helpers:node_key(NodeID)) of
        Self ->
            lager:info("registration successful, sending connected message"),
            erlang:spawn('legacy_msg_handler', 'connected_msg', [NodeID]),
            {'ok', State};
        _Other ->
            lager:error("registration failed, should be ~p got ~p", [Self, _Other]),
            {'stop', State}
    end;
websocket_info(_Info, State) ->
    lager:debug("unkown web socket info ~p", [_Info]),
    {'ok', State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), req(), state()) -> 'ok'.
terminate(_Reason, _Req, #state{nodeid=NodeID}) ->
    erlang:spawn('legacy_metrics', 'connections_count', [-1]),
    erlang:spawn('legacy_ota_worker', 'handle_node_reboot', [NodeID]),
    erlang:spawn('legacy_msg_handler', 'disconnected_msg', [NodeID]),
    lager:warning("terminated ws for: ~p because: ~p", [NodeID, _Reason]).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec register_node(binary()) -> 'ok' | 'error' | {'retire', pid()}.
register_node(NodeID) ->
    Self = self(),
    lager:debug("registering node ~p to ~p", [NodeID, Self]),
    case gproc:lookup_global_name(legacy_helpers:node_key(NodeID)) of
        'undefined' ->
            try gproc:add_global_name(legacy_helpers:node_key(NodeID)), 'ok' catch
                'error':_E ->
                    lager:error("failed to register node ~p : ~p", [NodeID, _E]),
                    'error'
            end;
        Self ->
            lager:debug("node ~p is already registered with ~p", [NodeID, Self]);
        Other ->
            lager:warning("node ~p is register with obselete process ~p", [NodeID, Other]),
            {'retire', Other}
    end.
