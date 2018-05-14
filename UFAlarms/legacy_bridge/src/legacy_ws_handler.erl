-module(legacy_ws_handler).

-behaviour(cowboy_websocket_handler).

-author("Gina Hagg <ghagg@sensity.com").

-export([
    init/3
    ,translatetoproto/1
    ,register_nodetype/2
]).

-export([
    terminate/3
    ,websocket_handle/3
    ,websocket_info/3
    ,websocket_init/3
    ,websocket_terminate/3
]).

-include("legacy.hrl").

-compile([{parse_transform, lager_transform}]).

-record(state,
  {nodeid, swid = [], orgid=[], siteid=[],firmware_on = 'false'}).

-define(CONNECTION_STATUS, <<"ConnectionStatus">>).
-define(BogusTimerMsg, {'SensorSampleReq', "gg"}).
-define(BogusSensorPayload, <<122, 4, 10, 2, 103, 103>>).
-define(DEFAULT_TMO, 3720000). %timeout for all is 62mins.
-define(PIGEON_3, <<"N03">>).
-define(VIDEO, <<"N02">>).
-define(LOGINTYPE, 1).
-define(TIMEREQTYPE, 13).

init({_, 'http'}, _Req, _Opts) ->
    {'upgrade', 'protocol', 'cowboy_websocket'}.

%%handles
websocket_init(_Any, Req, []) ->
    %lager:debug("Req: ~p",[Req]),
    {Nodeid, Req2} = cowboy_req:binding('node_id', Req),
    am_i_supposed_to_be_here(Nodeid),
    self() ! 'post_init',
    {'ok', Req2, #state{nodeid=Nodeid}}.

websocket_handle({'binary', Msg}, Req,
    State = #state{nodeid = Nodeid}) ->
    {EnvlpType, Rest} = type_of_msg(Msg),
    EMsgs = [build_response_to_node(EnvlpType, Nodeid)],
    self() ! {'register', EnvlpType, Rest},
    case EMsgs of
      [] -> {'ok', Req, State};
      _ ->
    Reply = [{'binary', Bin} || Bin <- EMsgs],
    {'reply', Reply, Req, State}
    end;
websocket_handle(_, Req, State) ->
    {'noreply', Req, State}.

%%infos
websocket_info('post_init', Req, State = #state{nodeid = Nodeid}) ->
    lager:info("node ~p connected with pid ~p", [Nodeid, self()]),
    keepalive_register(Nodeid),
    init_gproc_register(Nodeid),
    helpers:send_connected(Nodeid),
    {'ok', Req, State};
websocket_info({'mqtt', Topic, Payload}, Req, State = #state{nodeid = Nodeid}) ->
    case length(Payload) > 0 of
        'true' -> Reply = [{'binary', Msg} || Msg <- Payload],
        lager:info("transmitting command ~p from DD to ~p", [Topic, Nodeid]),
        {'reply', Reply, Req, State};
      _ ->
          lager:warning("empty command ~p from DD to ~p,  not transmitting", [Topic, Nodeid]),
          {'ok', Req, State}
    end;
websocket_info({register, ?LOGINTYPE, Rest}, Req, State = #state{nodeid = Nodeid}) ->
    lager:debug("try gproc register for login ~p",[Nodeid]),
    init_gproc_register(Nodeid),
    device_response_handler:register_and_forward_msg(?LOGINTYPE, Rest, Nodeid, false),
    {'ok', Req, State};

websocket_info({'register', EnvlpType, Rest}, Req, State = #state{nodeid = Nodeid}) ->
    lager:debug("forwarding message: ~p for node: ~p", [EnvlpType, Nodeid]),
    device_response_handler:forward_msg(EnvlpType, Rest, Nodeid),
    {'ok', Req, State};
websocket_info('mqtt', Req, State) ->
    lager:info("mqtt no message"),
    {'ok', Req, State};
websocket_info({'send_light_schedule', Schedules}, Req, State) ->
    lager:info("legacy:sending light schedules to node", []),
    Reply = [{'binary', Msg} || Msg <- Schedules],
    {'reply', Reply, Req, State};
%%You can detect the network latency by sending a ping with a timestamp and making the server echoing it in the pong.
%But if the communication is cut (ie: without proper TCP termination) it is hard to say it is cut till you try to send something
websocket_info({'are_you_there'}, Req, State) ->
    lager:debug("legacy:sending are_you_there message"),
    Reply = {'binary', ?BogusSensorPayload},
    {'reply', Reply, Req, State};
websocket_info({'i_am_extinct'}, Req, State = #state{nodeid = Nodeid}) ->
    lager:info("legacy:oops I have become extinct, better disappear for Nodeid ~p", [Nodeid]),
    {'shutdown', Req, State};
websocket_info({'i_am_not_supposed_to_be_here'}, Req, State = #state{nodeid = Nodeid}) ->
    lager:info("I must be a video node, can't be here.terminating connection ~p", [Nodeid]),
    {'shutdown', Req, State};
websocket_info({'registersiteorg', Orgid, Siteid}, Req, State = #state{nodeid = _Nodeid}) ->
    {'ok', Req, State#state{orgid=Orgid, siteid=Siteid}};
websocket_info({'identify_yourself'}, Req, State = #state{nodeid = Nodeid}) ->
    update_gproc(Nodeid),
    {'ok', Req, State};
websocket_info('register', Req, #state{nodeid=NodeId}=State) ->
    lager:debug("received a register request for ~p", [NodeId]),
    _ = init_gproc_register(NodeId),
    {'ok', Req, State};
websocket_info('kill', Req, State) ->
    lager:debug("received a kill request, shuting down"),
    {'shutdown', Req, State};
websocket_info({'DOWN', Ref, 'process', _Pid, _Reason}, Req, State) ->
    lager:debug("received a DOWN from ~p : ~p", [_Pid, _Reason]),
    _ = erlang:demonitor(Ref),
    self() ! 'register',
    lager:debug("demonitoring from ~p and sending register command", [_Pid]),
    {'ok', Req, State};
websocket_info(_, Req, State) -> {noreply, Req, State}.


%%terminates
websocket_terminate({'normal', 'timeout'}, Req, State = #state{nodeid = Nodeid}) ->
    lager:warning("Node ~p has timed out due to inactivity.  Shutting down connection", [Nodeid]),
    helpers:send_disconnected(Nodeid),
    {'ok', Req, State};
websocket_terminate({'normal', 'shutdown'}, Req, State = #state{nodeid = Nodeid}) ->
    lager:warning("shutting down the websocket connection ~p nodeid: ~p", [self(), Nodeid]),
    {'ok', Req, State};
websocket_terminate({'error', Reason}, Req, State = #state{nodeid = Nodeid}) ->
    lager:warning("node ~p disconnected ~p (~p)", [Nodeid, Reason, self()]),
    helpers:send_disconnected(Nodeid),
    {'ok', Req, State};
websocket_terminate(Reason, Req, State = #state{nodeid = Nodeid}) ->
    lager:warning("node ~p disconnected ~p (~p)", [Nodeid, Reason, self()]),
    helpers:send_disconnected(Nodeid),
    {'ok', Req, State}.

terminate(Reason, Req, State) ->
    lager:warning("terminate: socket error: Reason: ~p", [Reason]),
    {'ok', Req, State}.

%% ------------------------------------------------------------------
%%private functions
%% ------------------------------------------------------------------

translatetoproto(UnpackedMsgs) ->
    EncodedMsgs = unode_proto_handler:encode_msgs(UnpackedMsgs),
    lager:debug("Binary msg received: ~p", [EncodedMsgs]),
    EncodedMsgs.

init_gproc_register(NodeId) ->
    Self = self(),
    Key = {'n', 'l', NodeId},
    lager:debug("registering node ~p to ~p", [NodeId, Self]),
    case gproc:where(Key) of
        'undefined' ->
            try gproc:reg(Key, Self) catch
                'error':_E ->
                    lager:error("failed to register node ~p with ~p : ~p", [NodeId, Key, _E])
            end;
        Self ->
            lager:debug("node ~p is already registered with ~p", [NodeId, Self]);
        Other ->
            lager:info("node ~p is register with obselete process ~p", [NodeId, Other]),
            lager:debug("monitoring ~p and sending kill command", [Other]),
            _ = erlang:monitor('process', Other),
            Other ! 'kill'
    end,
    'ok'.


update_gproc(Nodeid) ->
    lager:debug("registering nodeid with gproc"),
    case gproc:where({'n', 'l', Nodeid}) of
        'undefined' ->
            lager:debug("node is not registered for ~p, we are registering", [Nodeid]),
            try gproc:reg({'n', 'l', Nodeid}, self()) catch
                error:badarg ->
                    lager:debug("error registering gproc node ~p for ~p", [Nodeid, self()])
            end;
        Other->
            case Other =:= self() of
                'true' ->
                    lager:debug("we are already registered for node ~p, with self ~p", [Nodeid, self()]);
                _ ->
                    lager:error("node is registered for with obsolete websocket ~p!!!!---, hopefully websocket caught this dead socket first ~p",[Other, Nodeid])
            end
    end.

register_nodetype(Envlp, Nodeid) ->
    {'LoginReq', _, _, Clitype, _, _, _, _, _, _, _, _, _, _} = Envlp,
    IsNode = gproc:where({'n', 'l', Nodeid}),
    case IsNode of 'undefined'
        -> lager:debug("can't register attributes for nodeid undefined pid ~p", [Nodeid]);
        _ ->
            try gproc:set_attributes({'n', 'l', Nodeid}, [{'ty', Clitype}]) catch
                'error':'badarg' ->
                    {lager:debug("error registering node clitype in gproc for ~p: and ~p", [Clitype, Nodeid])}
            end
    end.

keepalive_register(Nodeid) ->
    try gproc:reg({'p', 'l', 'keepalive'}) catch
        'error':'badarg' ->
            {lager:debug("error registering gproc keepalive for ~p: and ~p", [self(), Nodeid])}
    end.

  %case binary:match(Nodeid,[?PIGEON_3],[{scope,{0,3}}]) of nomatch -> ok; _->
  %  try gproc:reg({p,l,keepalive}) catch error:badarg -> {lager:info("error registering gproc keepalive for ~p",[self()])} end
  %end.


type_of_msg(Msg) ->
    {{EnvlpType, Rest}, _} = protobuffs:decode(Msg, 'bytes'),
    {EnvlpType, Rest}.

build_response_to_node(EnvlpType, Nodeid) ->
    case EnvlpType of
        ?LOGINTYPE ->
            lager:debug("sending LoginResp to Node:~p", [Nodeid]),
            unode_proto_handler:encode_login_resp();
        ?TIMEREQTYPE ->
            lager:debug("sending TimeResp to Node:~p", [Nodeid]),
            unode_proto_handler:encode_time_resp();
        _ -> []
    end.

am_i_supposed_to_be_here(Nodeid) ->
    case binary:match(Nodeid, [?VIDEO], [{'scope', {0, 3}}]) of
        'nomatch' -> 'ok';
        _ -> self() ! {'i_am_not_supposed_to_be_here'}
    end.
