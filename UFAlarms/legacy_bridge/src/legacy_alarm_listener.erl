-module(legacy_alarm_listener).
-author("ghagg@sensity.com").
-behaviour(gen_event).
-compile([{parse_transform, lager_transform}]).

%% API
-export([listen/0]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
  terminate/2]).


listen() ->
  gen_event:add_sup_handler(alarm_handler, ?MODULE, []),
  receive
    Msg -> lager:debug("Alarm handler listening: ~p, ~p~n",[listen, Msg])
  end.

init(_Args) ->
  lager:debug("{alarm_handler_set}"),
  {ok,[]}.

handle_event({set_alarm,{system_memory_high_watermark,_}},S) ->
  lager:notice("memory alarm high watermark!"),
  {ok,S};

handle_event({clear_alarm,system_memory_high_watermark},S) ->
  lager:notice("cleared high watermark alarm"),
  {ok,S};

handle_event(_Event, Pid) ->
  lager:debug("alarm_event ~p",[Pid]),
  {ok, Pid}.

handle_call(_, State) ->
  {ok, ok, State}.

handle_info(_Info, State) ->
  lager:debug("[alarm_info, _Info]"),
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.