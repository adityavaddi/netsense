-module(mynodes_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-record(state, {
}).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    io:format("i have been called"),
    MatchHead1 = '_',
    Guard = [],
    Result = ['$$'],
    Vals =  gproc:select(names,[{MatchHead1, Guard, Result}]),
    Nodes = [element(3,lists:nth(1,Y)) || Y <- Vals, lists:nth(1,Y) =/= {n,l,mqttc}],
    io:format("MYNODES table: nodes in server right now:: ~p~n",
	       [Nodes]),

    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/plain">>}],
        jsx:encode(Nodes),
        Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.