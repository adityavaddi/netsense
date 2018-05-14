-module(simple).

-behaviour(websocket_client_handler).

-export([
         start_link/0,
         init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

start_link() ->
    websocket_client:start_link("ws://127.0.0.1:8080/websocket", ?MODULE, []).

init([], _ConnState) ->
    websocket_client:cast(self(), {text, <<"connect">>}),
    {ok, 2}.

websocket_handle({pong, _}, _ConnState, State) ->
    {ok, State};

websocket_handle({text, Msg}, _ConnState, 50) ->
     io:format("50 protobuf formatted questions and received 50 protobuf formatted answers.. Closing shop.. ~p~n", [Msg]),
     {close, <<>>, "done"};

websocket_handle({text, Msg}, _ConnState, State) ->
    io:format("Received text msg ~p~n", [Msg]),
    {ok,State};

websocket_handle({binary, Msg}, _ConnState, 50) ->
     io:format("50 protobuf formatted questions and received 50 protobuf formatted answers.. Closing shop.. ~p~n", [Msg]),
     {close, <<>>, "done"};
    
websocket_handle({binary, Msg}, _ConnState, State) ->
    io:format("recvd binary:will send back ~p~n", [Msg]),
    {meta, MsgName, Bin} = meta_proto:decode_msg(Msg,meta),
    io:format("~p answer binary: ~p~n", [MsgName,Bin]),
    NextQuestion = (catch proto_handler:handle_msg(MsgName,Bin)),
    io:format("~p:NextQuestion: ~p~n", [MsgName,NextQuestion]),
    Send = meta_proto:encode_msg({meta,MsgName,NextQuestion}),
    io:format("~p:Send: ~p~n", [MsgName,Send]), 
    websocket_client:cast(self(), {binary, Msg});
    %timer:sleep(1000),
    %{reply, {text, <<"connect">>}, State +1};
    %{reply, [{binary, Msg}], State +1};

websocket_handle({_Any, _}, _ConnState, State) ->
    {ok, State}.

websocket_info(Msg, _ConnState, State) ->
    {reply, {text, <<"start:erlang message received">>}, State}.

websocket_terminate(Reason, _ConnState, State) ->
    io:format("Websocket closed in state ~p wih reason ~p~n",
              [State, Reason]),
    ok.







