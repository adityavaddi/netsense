-module(response_handler).
-behaviour(gen_server).
-compile(export_all).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([prepare_response/2, start_link/0, stop/0]).

-define(TOPIC_RESP_EXT, "dcc/resp").
-define(TOPIC_EVT_EXT, "dcc/cmd").

-compile([{parse_transform, lager_transform}]).


init([]) ->
    lager:info("response_handler:init"),
    {ok, []}.

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
prepare_response(DecodedMsg, Topic) -> gen_server:call(?MODULE, {prepare_response, DecodedMsg, Topic}).
stop() -> gen_server:call(?MODULE, stop).
store_msg(Msg, Topic) -> gen_server:call(?MODULE, {store_msg, Msg, Topic}).

handle_call({prepare_response, UnpackedMsg, Topic}, _From, State) -> 
    lager:info("handlecall:prepare_response is called with topic: ~p~n",[Topic]),
    {RespTopic,Pl} = prepare_payload(Topic, UnpackedMsg),
    lager:info("Pl is: ~p~n",[Pl]),
    Payload = case Pl of
        [] -> [];
        _ -> 
        %Tokens = topic_tokens(Topic),
        %RTopic = build_resptopic(Tokens),
        msgpack:pack(Pl)
    end,
    lager:info("Payload is: ~p and topic: ~p~n",[Payload, RespTopic]),
    {reply, {RespTopic, Payload}, State};

 
%%TopicTokens is like this:["TopicA","Cisco","EmeryVille","N01234","req"]
%%store_msg will go to a it's own database gen_server later on when we have socket to clojure.
handle_call({store_msg, UnpackedMsg, Topic}, _From, State) -> 
    Tokens = topic_tokens(Topic),
    %%4th one is NodeId
    NodeId = lists:nth(4,Tokens),
    %%Store transient in Dets for now.
    tempcache:dets_save_detail(NodeId, UnpackedMsg),
    {reply, ok, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%private

topic_tokens(Topic) ->
    TT = binary_to_list(Topic),
    string:tokens(TT,"/").

%%["TopicA","Cisco","EmeryVille","N01234","dcc","req","Login"]
%%Drop last and append "resp" for the response queue to publish to.
build_resptopic(Tokens)->
    Resptopic0 = helpers:setnth(5,Tokens,"dcc"),
    Resptopic1 = helpers:setnth(6,Resptopic0,"resp"),
    Resptopic2 = helpers:setnth(7,Resptopic1, "LoginResp"),
    Resptopic = string:join(Resptopic2, "/"),
    lager:info("resptopic:~p~n",[Resptopic]),
    Resptopic.

extract_topic_prefix(Topic) ->
    Tpc = binary_to_list(Topic),
    Pos = string:rstr(Tpc, "node"),
    Pre = string:substr(Tpc,1,Pos-1),
    lager:info("response_handler:extracted prefix ~p~n",[Pre]),
    Pre.

%%unpacked msg is this format:
%%#{<<"ftype">> => <<"Volatile">>,<<"level">> => 0,<<"mask">> => 1,<<"name">> => <<"LightingForceState">>,
%%<<"nodeId">> => <<"N01232ea5">>,<<"pri">> => 3,<<"qualifiers">> => <<"undefined">>}}
prepare_payload(Topic, UnpackedMsg) -> 
    lager:info("preparepayload for topic:~p~n",[Topic]),
    Msgtype = maps:get(<<"name">>, UnpackedMsg),
    Nodeid = helpers:nodeid_from_topic(Topic),
    %%check if msgtype will be an atom or need to be converted later on.
    respond_accordingly(binary_to_atom(Msgtype, unicode),Nodeid,UnpackedMsg).

respond_accordingly('TimeReq',Nodeid, _UnpackedMsg) ->
    Pre = helpers:get_prefix(Nodeid),
    RTopic = list_to_binary([Pre,?TOPIC_RESP_EXT]),
    Timestamp = os:system_time(micro_seconds),
    {RTopic,#{<<"name">> => <<"TimeResp">>, <<"nodeid">> => [Nodeid], <<"time">> => Timestamp}};

%respond_accordingly('LoginReq',Nodeid, _UnpackedMsg) ->
%    Pre = helpers:get_prefix(Nodeid),
%    RTopic = list_to_binary([Pre,?TOPIC_RESP_EXT]),
%    Timestamp = os:system_time(micro_seconds),
%    {RTopic, #{<<"name">> => <<"LoginResp">>, <<"nodeid">> => [Nodeid], <<"ok">> => true, <<"time">> => Timestamp}};

%%send generic 
respond_accordingly(_, _Nodeid,UnpackedMsg) ->
    lager:info("we don't respond to this message, ~p~n", [UnpackedMsg]),
    {"",[]}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

    



