-module(message_processor).
-author("Chiradip Mandal").
-author("Gina Hagg").
-compile(export_all).
%% -export([decoder/3, selector/0]).
-compile([{parse_transform, lager_transform}]).

selector() ->
  lager:info("*** 1Selector: before recieve loop~n"),
  receive
    {selector, Topic, Payload} ->
      unpack_payload(Topic, Payload);
    _ ->
      lager:warning("Not handling this.")
  end.

unpack_payload(Topic, Payload) ->
  lager:info("*** unpack_payload: ~p: ~p ~n", [Topic, Payload]),
  UnpackedM = msgpack:unpack(Payload),
  case UnpackedM of
    {ok, NodeMsg} ->
      lager:info("*** Message from ~p: ~p ~p~n", [Topic, NodeMsg, self()]);
      %selector(Topic, NodeMsg);
    {error, E} ->
      lager:error("Error is: ~p~n", [E])
  end.

selector(Topic, NodeMsg) ->
  lager:info("*** 2Selector: ~p: ~p ~p~n", [Topic, NodeMsg, self()]).
  

presence_processor([_ | SubTopic], NodeMsg) -> % omit the "/" that came with the SubTopic
  lager:info("Presence Processor is called ~p : ~p~n", [SubTopic, NodeMsg]),
  [Customer, Site, Device | _] = string:tokens(SubTopic, "/"),
  lager:info("Customer: ~p~n", [Customer]),
  lager:info("Site: ~p~n", [Site]),
  lager:info("Device: ~p~n", [Device]),
  {presence, Customer, Site, Device}.

presence_state() ->
  receive
        {something} -> {}
  end.

