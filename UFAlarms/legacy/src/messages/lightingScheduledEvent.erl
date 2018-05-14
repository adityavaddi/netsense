%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Lighting Schedule Event ==
%% Lighting Schedule Event message API
%% @end
%%%-------------------------------------------------------------------
-module(lightingScheduledEvent).

-export([
    to_map/1
    ,to_record/1
    ,create/3
    ,key/2
]).

-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type 'LightingScheduledEvent'() :: #'LightingScheduledEvent'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert LightingScheduledEvent record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('LightingScheduledEvent'()) -> map().
to_map(LSE) ->
    #{
        "name" => "LightingScheduledEvent"
        ,"id" => LSE#'LightingScheduledEvent'.id
        ,"event" => calendarEvent:to_map(LSE#'LightingScheduledEvent'.event)
        ,"state" => lightingCtrl:to_map(LSE#'LightingScheduledEvent'.state)

    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert LightingScheduledEvent record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'LightingScheduledEvent'().
to_record(Map) ->
    #'LightingScheduledEvent'{
        id = maps:get("id", Map)
        ,event = calendarEvent:to_record(maps:get("event", Map))
        ,state = lightingCtrl:to_record(maps:get("state", Map))
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(integer(), any(), any()) -> 'LightingScheduledEvent'().
create(ID, Event, State) ->
    #'LightingScheduledEvent'{
        id = ID
        ,event = Event
        ,state = State
    }.

%%--------------------------------------------------------------------
%% @doc
%% Return routing key
%% @end
%%--------------------------------------------------------------------
-spec key(binary(), any()) -> binary().
key(_NodeID, _) -> <<>>.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

to_map_test() ->
    LightingScheduledEvent = #'LightingScheduledEvent'{
        id = 1
        ,event = #'CalendarEvent'{
            sec = 12
            ,min = 12
            ,hr = 11
            ,wday = 1
            ,mday = 1
            ,mon = 1
            ,year = 2017
        }
        ,state = #'LightingCtrl'{
            pri = 1
            ,mask = 55
            ,level = <<0>>
            ,qualifiers = 12
        }
    },
    Map = #{
        "name" => "LightingScheduledEvent"
        ,"id" => 1
        ,"event" => #{
            "name" => "CalendarEvent"
            ,"sec" => 12
            ,"min" => 12
            ,"hr" => 11
            ,"wday" => 1
            ,"mday" => 1
            ,"mon" => 1
            ,"year" => 2017
        }
        ,"state" => #{
            "name" => "LightingCtrl"
            ,"pri" => 1
            ,"mask" => 55
            ,"level" => <<0>>
            ,"qualifiers" => 12
        }
    },
    ?assertEqual(Map, to_map(LightingScheduledEvent)),
    'ok'.

to_record_test() ->
    LightingScheduledEvent = #'LightingScheduledEvent'{
        id = 1
        ,event = #'CalendarEvent'{
            sec = 12
            ,min = 12
            ,hr = 11
            ,wday = 1
            ,mday = 1
            ,mon = 1
            ,year = 2017
        }
        ,state = #'LightingCtrl'{
            pri = 1
            ,mask = 55
            ,level = <<0>>
            ,qualifiers = 12
        }
    },
    Map = #{
        "name" => "LightingScheduledEvent"
        ,"id" => 1
        ,"event" => #{
            "name" => "CalendarEvent"
            ,"sec" => 12
            ,"min" => 12
            ,"hr" => 11
            ,"wday" => 1
            ,"mday" => 1
            ,"mon" => 1
            ,"year" => 2017
        }
        ,"state" => #{
            "name" => "LightingCtrl"
            ,"pri" => 1
            ,"mask" => 55
            ,"level" => 0
            ,"qualifiers" => 12
        }
    },
    ?assertEqual(LightingScheduledEvent, to_record(Map)),
    'ok'.

create_test() ->
    Event = calendarEvent:create(12, 12, 11, 1, 1, 1, 2017),
    State = lightingCtrl:create(1, 55, 0, 12),
    ?assertMatch(#'LightingScheduledEvent'{
        id = 1
        ,event = #'CalendarEvent'{
            sec = 12
            ,min = 12
            ,hr = 11
            ,wday = 1
            ,mday = 1
            ,mon = 1
            ,year = 2017
        }
        ,state = #'LightingCtrl'{
            pri = 1
            ,mask = 55
            ,level = <<0>>
            ,qualifiers = 12
        }
    }, create(1, Event, State)).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
