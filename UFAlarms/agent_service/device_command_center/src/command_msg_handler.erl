-module(command_msg_handler).
-compile(export_all).
-compile([{parse_transform, lager_transform}]).
-include_lib("eunit/include/eunit.hrl").


-define(LegacyLightingTopic, "lighting").
-define(LegacyConfigTopic, "config").
-define(LegacyScheduleTopic, "schedules").
-define(LegacyFirmwareTopic, "firmware").
-define(TOPIC_CMD_EXT, "dcc/cmd/").
-define(UNODE,<<"N01">>).
-define(PIGEON,<<"N03">>).
-define(VIDEO,<<"N02">>).

inspect_adapt_command(Data, Msg) ->
	Msgtype = case maps:is_key("name", Data) of true -> maps:get("name", Data, Msg); _-> maps:get(<<"name">>, Data) end,
  %Nodeid = case maps:is_key("nodeid", Data) of true -> maps:get("nodeid",Data); _-> maps:get(<<"nodeid">>, Data) end,
  %Type = type_of_transport(Nodeid),
  Subscriber = helpers:find_subscriber(Msgtype),
  {direct, Subscriber, Data}.
  %case Type of mqtt ->
	% adapt_command(Msgtype, Nodeid, Msg);
  %_-> {direct, Subscriber, Data}
  %end.

adapt_command(Msgtype, Nodeids, Msg) -> 
  %{LegacyNodes,Others} = Nodeids,
  %Topics = store_groups_and_create_topics(new, Msgtype, Others),
  LegacyTopic = store_groups_and_create_topics(legacy, Msgtype, Nodeids),
  lager:info("LegacyTopics: ~p~n",[LegacyTopic]),
  {mqtt, LegacyTopic, Msg}.

store_groups_and_create_topics(legacy, Msgtype, _) ->
    Subscriber = helpers:find_subscriber(Msgtype),
    iolist_to_binary([get_prefix() ,"dcc/cmd/", Subscriber]);

store_groups_and_create_topics(new, Msgtype, Nodeids) ->
        [iolist_to_binary([get_prefix(),Nodeid,?TOPIC_CMD_EXT,Msgtype]) || Nodeid <- Nodeids].

%to be replace by call to mnesia
get_prefix() ->
    "TopicA/Cisco/Emeryville/".

type_of_transport(Nodeids) ->
  Nodeid = lists:nth(1, Nodeids),
  case binary:match(Nodeid,[?VIDEO],[{scope,{0,3}}]) of nomatch -> direct; _-> mqtt end.