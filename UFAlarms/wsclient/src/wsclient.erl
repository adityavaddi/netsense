-module(wsclient).

-behaviour(websocket_client_handler).

-record(state, {
          buffer = [] :: list(),
          waiting = undefined :: undefined | pid()
         }).

-define(NumOfMsgsToBeTransmitted,4).
%-define(WEBSOCKET_SERVER_URL, "wss://localhost:10443/device/N01232ea5").
-define(WEBSOCKET_SERVER_URL, "ws://localhost:8080/device/N01232ea5").
-define(WEBSOCKET_SERVER_URL1, "ws://localhost:8080/device/N013341e5").
-define(WEBSOCKET_SERVER_URL2, "ws://localhost:8080/device/N013341e0").
-define(PUSH_URL, "wss://localhost:10443/device/N01232ea5/ws/push").
-define(EX_PUSH_URL, "wss://192.168.65.255:10443/device/N01232ea5/ws/push").
-define(LoginReq_NS1_1, <<10,19,10,5,78,83,49,95,49,16,1,26,8,117,110,111,100,101,45,118,51>>).
-define(EmptyEnvelope,{'Envelope',undefined,
            undefined,undefined,undefined,undefined,undefined,
            undefined,undefined,undefined,undefined,undefined,
            undefined,undefined,undefined,undefined,undefined,
            undefined,undefined,undefined,undefined,undefined,
            undefined,undefined}).

-compile(export_all).
-compile([{parse_transform, lager_transform}]).

%%start communication by sending this factorial question below. Server will parse and send us an answer.
%%factorial of 2 = {meta,factorial,{factorial,2,0}
%{'LoginReq',"N01232ea5",0,"unode-v4","5549c61","SensityDefault","x",60,"a8733d94","10.20.108.92",1447470224149108}
-define(LoginReqStart,<<10,105,10,9,78,48,49,50,51,50,101,97,53,16,0,26,8,117,110,111,100,101,45,118,52,34,7,53,53,52,57,99,54,49,42,14,83,101,110,115,105,116,121,68,101,102,97,117,108,116,50,1,120,56,60,66,8,97,56,55,51,51,100,57,52,74,12,49,48,46,50,48,46,49,48,56,46,57,50,80,244,148,162,228,246,142,201,2,90,17,56,52,58,66,56,58,48,50,58,66,51,58,65,56,58,55,66>>).
-define(SensorSampleStart,<<82,19,10,2,108,116,16,232,165,241,209,210,245,199,2,24,58,34,2,112,102>>).
%%'Messages':decode_msg(<<10,9,8,3,16,1,26,1,25,32,0,16,2>>,'LightingForceState').
%%{'LightingForceState',{'LightingCtrl',3,1,<<25>>,0},'Volatile'}
-define(LightingForceStateStart,<<10,9,8,3,16,1,26,1,25,32,0,16,2>>).
-define(NetworkAlarmStart, <<10,2,57,57,16,1,24,1,34,6,98,108,97,98,108,97>>).
-define(DeviceAlarmMessage,<<58,7,8,230,1,16,0,26,0>>).
start_link() ->
    start_link(?WEBSOCKET_SERVER_URL).

start_link1() ->
    start_link(?WEBSOCKET_SERVER_URL1).

start_link2() ->
    start_link(?WEBSOCKET_SERVER_URL2).

startnode(Nodeid) ->
    Url = "ws://localhost:8080/device/" ++ Nodeid,
    start_link(Url).


build_login_req(Nodeid) ->
    lager:info("Nodeid:~p~n",[Nodeid]),
    Timestamp = os:system_time(micro_seconds),
    LoginReq = {'LoginReq',Nodeid, 0,"unode-v3","6ab8efe","SensityQA01","x",149,"2d80cd53","192.168.64.213",Timestamp,"8A:15:54:CC:52:32","AC:3F:A4:33:06:18",'WPA2_PSK'},
      lager:info("LoginReq:~p~n",[LoginReq]),
     NewEvlp = setelement(2,?EmptyEnvelope,LoginReq),
    'Messages':encode_msg(NewEvlp).

build_login_req(Nodeid,Version) ->
    lager:info("Nodeid:~p~n",[Nodeid]),
    Timestamp = os:system_time(micro_seconds),
    LoginReq = {'LoginReq',Nodeid, 0,"unode-v3",Version,"SensityQA01","x",149,"2d80cd53","192.168.64.213",Timestamp,"8A:15:54:CC:52:32","AC:3F:A4:33:06:18",'WPA2_PSK'},
      lager:info("LoginReq:~p~n",[LoginReq]),
     NewEvlp = setelement(2,?EmptyEnvelope,LoginReq),
    'Messages':encode_msg(NewEvlp).


build_device_alarm_233() ->
    DA = {'DeviceAlarm','HWFail_STUCK_RELAY','Clear',[]},
    NewEvlp = setelement(8,?EmptyEnvelope, DA),
    'Messages':encode_msg(NewEvlp).

build_device_alarm_232() ->
    DA = {'DeviceAlarm','SWUpdateFail_SENSORPOD','Clear',[]},
    NewEvlp = setelement(8,?EmptyEnvelope, DA),
    'Messages':encode_msg(NewEvlp).

startnewlogin(Nodeid) ->
    start_apps(),
    lager:start(),
    Url = "ws://localhost:8080/device/" ++ Nodeid,
    EncodedEvlp = build_login_req(Nodeid),
    R = websocket_client:start_link(Url, ?MODULE, []),
    case R of
        {ok, Pid} ->
            send_binary(Pid,EncodedEvlp);
        {error, E} -> lager:info("Error: ~p~n", [E])
    end,
    ok.

undefined_sample() ->
 %{'LoginReq',"N01334075",0,"unode-v4","5f24a52","huawei-2.4","x","11","3b3bd8dd","192.168.67.141",1466120797339569,"2C:AB:00:B7:0B:80",undefined,'WPA2_PSK'}.
  {'LoginReq',"N01334075",0,"unode-v4","5f24a52","huawei-2.4","x",11,"3b3bd8dd","192.168.67.141",1466120797339569,"2C:AB:00:B7:0B:80",undefined,'WPA2_PSK'}.

startvideo(Nodeid) ->
    start_apps(),
    lager:start(),
    Url = "ws://localhost:8080/device/" ++ Nodeid,
    EncodedEvlp = build_login_req(Nodeid),
    R = websocket_client:start_link(Url, ?MODULE, []),
    case R of
        {ok, Pid} ->
            send_binary(Pid,EncodedEvlp);
        {error, E} -> lager:info("Error: ~p~n", [E])
    end,
    ok.

start_link(Url) ->
    lager:start(),
    R = websocket_client:start_link(Url, ?MODULE, []),
    case R of
        {ok, Pid} ->
            send_binary(Pid,?LoginReqStart);
        {error, E} -> lager:info("Error: ~p~n", [E])
    end,
    ok.

startlinks() ->
    Url = ?WEBSOCKET_SERVER_URL,
    start_apps(),
    lager:start(),
    R = websocket_client:start_link(Url, ?MODULE, []),
    LoginReq = build_login_req("N0gina3"),
    DA = build_device_alarm_233(),
    DA2 = build_device_alarm_232(),
    case R of
        {ok, Pid} ->
            send_binary(Pid,LoginReq),
            timer:sleep(1000),
            send_binary(Pid, DA),
            timer:sleep(1000),
            send_binary(Pid,DA2);
            %timer:sleep(1000)
            %send_binary(Pid, ?SensorSampleStart);
           % timer:send_after(2000, Pid, ?NetworkAlarmStart),
           % timer:send_after(2000, Pid, ?LightingForceStateStart),
           % timer:send_after(2000, Pid, ?SensorSampleStart);
        {error, E} -> io:format("Error: ~p~n", [E])
    end,
    ok.

start_websockets_test(Number) ->
    start_apps(),
    lager:start(),
    [
        begin
            lager:info("sending ~p~n",[N]),
            timer:sleep(2000),
            Node = iolist_to_binary(["N0e", integer_to_list(N)]),
            Url = iolist_to_binary(["ws://localhost:8080/device/", Node]),
            LoginReq = {'LoginReq', 0, "unode-v3", "6ab8efe", "SensityQA01", "x", 149,"2d80cd53", "192.168.64.213", 1458859246192382,"8A:15:54:CC:52:32", "AC:3F:A4:33:06:18",'WPA2_PSK'},
           % LoginReq = {'LoginReq',Node,0,"unode-v4","5549c61","SensityDefault","x",60,"a8733d94","10.20.108.92",1447470224149108},
            NewEvlp = setelement(2,?EmptyEnvelope,LoginReq),
            EncodedEvlp = 'Messages':encode_msg(NewEvlp),
            R = websocket_client:start_link(Url, ?MODULE, []),
            case R of
                {ok, Pid} ->
                    send_binary(Pid,EncodedEvlp);
                {error, E} -> lager:info("Error: ~p~n", [E])
            end
        end
     || N <- lists:seq(1,Number)
    ],
ok.

start_websockets_test_local(Number) ->
    start_apps(),
    lager:start(),
    [
        begin
            lager:info("sending ~p~n",[N]),
            timer:sleep(2000),
            Node = "N0e" ++ integer_to_list(N),
            %Url = iolist_to_binary(["wss://127.0.0.1:8080/device/", Node]),
            Url = "ws://localhost:8080/device/" ++ Node,
            EncodedEvlp = build_login_req(Node),
            R = websocket_client:start_link(Url, ?MODULE, []),
            case R of
                {ok, Pid} ->
                    send_binary(Pid,EncodedEvlp);
                {error, E} -> lager:info("Error: ~p~n", [E])
            end
        end
     || N <- lists:seq(1,Number)
    ],
ok.

sendnewversion(Number,Version) ->
    start_apps(),
    lager:start(),
    [
        begin
            lager:info("sending ~p~n",[N]),
            timer:sleep(2000),
            Node = "N0e" ++ integer_to_list(N),
            %Url = iolist_to_binary(["wss://127.0.0.1:8080/device/", Node]),
            Url = "ws://localhost:8080/device/" ++ Node,
            EncodedEvlp = build_login_req(Node,Version),
            R = websocket_client:start_link(Url, ?MODULE, []),
            case R of
                {ok, Pid} ->
                    send_binary(Pid,EncodedEvlp);
                {error, E} -> lager:info("Error: ~p~n", [E])
            end
        end
     || N <- lists:seq(1,Number)
    ],
ok.

start_link_login(Url) ->
    lager:start(),
    R = websocket_client:start_link(Url, ?MODULE, []),
    case R of
        {ok, Pid} ->
            send_binary(Pid,?LoginReqStart);
        {error, E} -> io:format("Error: ~p~n", [E])
    end,
    ok.

start_link_push() ->
    start_apps(),
    lager:start(),
     R = websocket_client:start_link(?PUSH_URL, ?MODULE, []),
    lager:info("~p~n",[R]),
    case R of
        {ok, Pid} ->
            send_binary(Pid,?SensorSampleStart);
        {error, E} -> io:format("Error: ~p~n", [E])
    end,
    ok.

start_ex_push() ->
    start_apps(),
    lager:start(),
     R = websocket_client:start_link(?EX_PUSH_URL, ?MODULE, []),
    lager:info("~p~n",[R]),
    case R of
        {ok, Pid} ->
            send_binary(Pid,?SensorSampleStart);
        {error, E} -> io:format("Error: ~p~n", [E])
    end,
    ok.

%%for common_test don't send the first package.
start_link(Url,test) ->
    {ok,_Pid} = websocket_client:start_link(Url, ?MODULE, []).

stop(Pid) ->
    Pid ! stop.

init(_, _WSReq) ->
    {ok, #state{}}.

start_apps() ->
application:start(sasl),
    application:start(asn1),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl).


%%if we reached the NumOfMsgsToBeTransmitted, we will stop. For normal it is 15, for testing it is 1. One message and stop.
stop_if_lastone(no_more_questions) ->
    send_text(self(),<<"no_more_questions">>),
    {close, <<"Finished transmitting. Closing Shop">>, []};

stop_if_lastone(Resp) ->
    lager:info("after sleeping for a second, sending binary:~p to selfpid ~p ~n",[Resp, self()]),
    timer:sleep(1000),
    send_binary(self(), Resp).

send_binary(Pid, Msg) ->
    lager:info("Sending binary msg: ~p~n",[Msg]),
    websocket_client:cast(Pid, {binary, Msg}).

websocket_handle(Frame, _, State = #state{waiting = undefined, buffer = Buffer}) ->
    lager:info("1.Client received frame ~p ~n",[Frame]),
    %Expected = <<131,164,110,97,109,101,169,76,111,103,105,110,82,101,115,112,162,111,107,195,164,116,105,109,101,207,0,5,36,135,159,96,172,12>>,
    Expected = [{binary,<<18,11,8,1,16,173,177,176,128,139,170,201,2>>}],
    ExpectedResp = [{<<"name">>,<<"LoginResp">>},{<<"ok">>,true},{<<"time">>,1447539796651020}],
    case Frame == Expected of
        true -> lager:info("We got a response: ~p", [ExpectedResp]);
        _->  lager:info("We got another response: ~p", [Frame])
    end,
    BufferLength = length(Buffer),
    lager:info("We have ~p items in buffer.",[BufferLength+1]),
    %send_binary_frame(Frame, BufferLength),
    {ok, State#state{buffer = [Frame|Buffer]}};

websocket_handle(_Frame, _, State = #state{waiting = _From}) ->
     {ok, State#state{waiting = undefined}}.

websocket_info({send_text, Text}, WSReq, State) ->
    websocket_client:send({text, Text}, WSReq),
    {ok, State};

websocket_info({recv, From}, _, State = #state{buffer = []}) ->
    {ok, State#state{waiting = From}};

websocket_info({recv, From}, _, State = #state{buffer = [Top|Rest]}) ->
    From ! Top,
    {ok, State#state{buffer = Rest}};

websocket_info(stop, _, State) ->
    {close, <<>>, State}.

websocket_terminate(Close, Reason, State) ->
    io:format("Websocket closed with frame ~p and reason ~p and state ~p", [Close, Reason,State]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_binary_frame({binary,Bin}, BufferLength) when BufferLength < ?NumOfMsgsToBeTransmitted ->
    store_payload(Bin),
    Resp = process_incoming_binary_and_prepare_next_question(Bin),
    stop_if_lastone(Resp);

send_binary_frame({binary,_},_BufferLength) ->
    stop_if_lastone(no_more_questions);

send_binary_frame({text,_}, _) ->
    lager:info("server responded with stop_sending_me_garbage. we are done folks, we closed shop!!."),
    not_handling_text.

decode_payload(Msg) ->
    {meta, MsgName, Bin} = meta_proto:decode_msg(Msg,meta),
    lager:info("~p answer binary: ~p~n", [MsgName,Bin]),
    {MsgName, Bin}.

process_incoming_binary_and_prepare_next_question(Msg)->
    {meta, MsgName, DecodedAnswer} = proto_handler:decode_incoming_payload(Msg),
    NextQuestion = (catch proto_handler:decode_answer_and_get_next_question(MsgName,DecodedAnswer)),
    EncodedNextQuestion = case NextQuestion of
        no_more_questions -> no_more_questions;
        _-> proto_handler:encode_next_outgoing_question(MsgName,NextQuestion)
    end,
    lager:info("~p:Sending encode next question: ~p~n", [MsgName,EncodedNextQuestion]),
    EncodedNextQuestion.

encode_msg_for_sendoff(Number) ->
    proto_handler:encodeFactorialMessage(Number).

store_payload(Bin) ->
    {ok,Ref} = dets:open_file(?MODULE,[]),
    dets:insert(Ref,{payload,Bin}),
    dets:close(Ref).

cleanup_store() ->
    {ok,Ref} = dets:open_file(?MODULE,[]),
    dets:delete_all_objects(Ref),
    dets:close(Ref).

fetch_last_received() ->
    {ok,Ref} = dets:open_file(?MODULE,[]),
    Payload = dets:lookup(Ref,payload),
    dets:close(Ref),
    Payload.

%%%%%%%%%%%%%% for common_test testing %%%%%%%%%%%%%%%

recv(Pid) ->
    recv(Pid, 5000).

recv(Pid, Timeout) ->
    Pid ! {recv, self()},
    receive
        M -> lager:info("receive:M is ~p~n",[M]),
        M
    after
        Timeout -> error
    end.

send_text(Pid, Msg) ->
    websocket_client:cast(Pid, {text, Msg}).

send_ping(Pid, Msg) ->
    websocket_client:cast(Pid, {ping, Msg}).

%unode_msg_samples() ->
%    SensorSampleT = {"t", randPercentageByValue(21400), "fahrenheigt", System.currentTimeMillis * 1000},
%    SensorSampleP = {"p", Random.nextInt(2), "n/a", System.currentTimeMillis * 1000},
%    SensorSampleL = {"l", curEmittedLight, "lum", System.currentTimeMillis * 1000}.


%unode_proto_handler:encode_env_by_type('DeviceAlarm',{'DeviceAlarm','HWFail_EEPROM','Clear',[]}, <<"N01232ea3">>).
%<<58,7,8,230,1,16,0,26,0>>
 %   {'DeviceAlarm', {'DeviceAlarm','HWFail_EEPROM','Clear',[]}, <<"N01232ea3">>}
 %'Messages':decode_msg(<<10,9,8,3,16,1,26,1,25,32,0,16,2>>,'LightingForceState').
%{'LightingForceState',{'LightingCtrl',3,1,<<25>>,0},
 %                     'Volatile'}
 device_alarm_message() ->
    <<58,7,8,230,1,16,0,26,0>>.


