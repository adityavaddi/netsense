-module(command_distributor).

-author("Gina Hagg ghagg@sensity.com").

-behaviour(gen_server).
-behaviour(poolboy_worker).

-compile([{parse_transform, lager_transform}]).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([findConcernedNodes/1, start_link/0, start_link/1, stop/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([code_change/3, handle_call/3, handle_cast/2,
     handle_info/2, init/1, terminate/2]).

-record(state, {off=[], on =[]}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
stop() ->
    lager:debug("stop is called"),
    gen_server:call(?MODULE, stop).

start_link(Args) ->
    lager:debug("~p is starting",[?MODULE]),
    gen_server:start_link(?MODULE, Args, []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
              []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(_Args) ->
    pg2:join(distributor_group,self()),
    lager:debug("joined distributor group"),
    {ok, #state{}}.

%%calls
handle_call(stop, _From, State) ->
    lager:debug("stopping ~p..", [?MODULE]),
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({on,Nodeid}, #state{on = On}) ->
    lager:debug("Nodeid signed in: ~p~n",[Nodeid]),
    {noreply, #state{on = On + 1}};

handle_info({colony}, State) ->
    lager:debug("received request for "
           "colony info~n"),
    Reply = gproc:lookup_values({p, l, colony}),
    {reply, Reply, State};

%[{<0.64.0>,"N01334ec"},{<0.53.0>,"N01443ae"}]
handle_info({command,Payload}, State) ->
    lager:debug("got a command from listener"),
    spawn(fun() -> distribute(Payload) end),
    {noreply, State};

handle_info(post_init, State) ->
   {noreply, State};

handle_info({shutdown, tcp_closed}, State) ->
    lager:debug("info shutdown"), {noreply, State};

handle_info(_Msg, State) -> {noreply, State}.

terminate({shutdown, tcp_closed}, State) ->
    lager:debug("terminate, shutdown"), {ok, State};
terminate(Reason, _State) ->
    lager:debug("terminate, reason ~p~n", [Reason]), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% private
%% ------------------------------------------------------------------

distribute(Payload) ->
  {Subject,Data} = handle_packed(Payload),
  BData = deal_with_binary_strings(Data),
  Nodeids = case maps:is_key("nodeid", BData) of
        true -> maps:get("nodeid", BData);
        _ -> maps:get(<<"nodeid">>, BData)
          end,
  distribute_to_handlers(Subject,Nodeids, BData).

distribute(Subject,Payload) ->
  Data = deal_with_binary_strings(Payload),
  Nodeids = case maps:is_key("nodeid", Data) of
        true -> maps:get("nodeid", Data);
        _ -> maps:get(<<"nodeid">>, Data)
          end,
  distribute_to_handlers(Subject,Nodeids, Data).

distribute_to_handlers("schedules", Nodeids, UnpackedMsgs) ->
    %{ok ,UnpackedMsgs} = msgpack:unpack(Payload),
    Msgname = case maps:is_key("name", UnpackedMsgs) of
        true -> maps:get("name", UnpackedMsgs);
        _ -> maps:get(<<"name">>, UnpackedMsgs)
          end,
    lager:debug("schedules_subscriber:unpacked message: ~p",
           [UnpackedMsgs]),
    EncodedMsgs =
    lighting_handler:prepare_schedules(UnpackedMsgs),
    ProtoBinary = case is_list(EncodedMsgs) of
            true -> EncodedMsgs;
            _ -> [EncodedMsgs]
          end,
    subscriber_helper:send_to_nodes(ProtoBinary, Msgname,
                    Nodeids, UnpackedMsgs);
distribute_to_handlers("firmware", _Nodeids, Payload) ->
    spawn_link(fun () ->
               legacy_fw_worker:unpack_protify_send(Payload)
           end);
distribute_to_handlers("config", _Nodeids, Payload) ->
    spawn_link(fun () ->
               legacy_config_worker:unpack_protify_send(Payload)
           end);
distribute_to_handlers(Subject, _Nodeids, Payload) ->
    spawn_link(fun () ->
               subscriber_helper:unpack_protify_send(Subject, Payload)
           end).

findConcernedNodes(UnpackedMsgs) ->
    Nodeids = case maps:is_key("nodeid", UnpackedMsgs) of
        true -> maps:get("nodeid", UnpackedMsgs);
        _ -> maps:get(<<"nodeid">>, UnpackedMsgs)
          end,
    Colony = gproc:lookup_values({p, l, colony}),
    PidNids = [{Pid, Nid}
           || {Pid, Nid} <- Colony, lists:member(Nid, Nodeids)],
    lager:debug("These nodes are in this server. Rest "
           "we ignore ~p~n",
           [PidNids]),
    PidNids.

deal_with_binary_strings(Payload) ->
  Data2 = maps:to_list(Payload),
  Data3 = lists:map(fun(X)-> {Y,Z} = X, case is_binary(Y) of true -> {binary_to_list(Y),Z}; _-> {Y,Z}  end end, Data2),
  maps:from_list(Data3).

handle_packed(Msg) ->
  {ok,Data1} = msgpack:unpack(Msg),
    Data2 = maps:to_list(Data1),
    Data3 = lists:map(fun(X)-> {Y,Z} = X, case is_binary(Y) of true -> {binary_to_list(Y),Z}; _-> {Y,Z}  end end, Data2),
    Data = maps:from_list(Data3),
    Name = case maps:is_key("name", Data) of true -> maps:get("name", Data); _-> maps:get(<<"name">>, Data) end,
    Topic = helpers:find_subscriber(Name), 
    lager:debug("Decoded message:~p sending to ~p~n", [Data,Topic]),
    {Topic, Data}.