%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Lighting Schedule Event ==
%% Lighting Schedule Event message API
%% @end
%%%-------------------------------------------------------------------
-module(lightingAstronomicalEvent).

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

-type 'LightingAstronomicalEvent'() :: #'LightingAstronomicalEvent'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert LightingAstronomicalEvent record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('LightingAstronomicalEvent'()) -> map().
to_map(LAE) ->
    #{
        "name" => "LightingAstronomicalEvent"
        ,"id" => LAE#'LightingAstronomicalEvent'.id
        ,"event" => 'astronomicalEvent':to_map(LAE#'LightingAstronomicalEvent'.event)
        ,"state" => lightingCtrl:to_map(LAE#'LightingAstronomicalEvent'.state)

    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert LightingAstronomicalEvent record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'LightingAstronomicalEvent'().
to_record(Map) ->
    #'LightingAstronomicalEvent'{
        id = maps:get("id", Map)
        ,event = 'astronomicalEvent':to_record(maps:get("event", Map))
        ,state = lightingCtrl:to_record(maps:get("state", Map))
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(integer(), any(), any()) -> 'LightingAstronomicalEvent'().
create(ID, Event, State) ->
    #'LightingAstronomicalEvent'{
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
    LightingAstronomicalEvent = #'LightingAstronomicalEvent'{
        id = 1
        ,event = #'AstronomicalEvent'{
            body = 'Sun'
            ,atype = 'Rise'
            ,zenith = 'Official'
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
        "name" => "LightingAstronomicalEvent"
        ,"id" => 1
        ,"event" => #{
            "name" => "AstronomicalEvent"
            ,"body" => "Sun"
            ,"atype" => "Rise"
            ,"zenith" => "Official"
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
    ?assertEqual(Map, to_map(LightingAstronomicalEvent)),
    'ok'.

to_record_test() ->
    LightingAstronomicalEvent = #'LightingAstronomicalEvent'{
        id = 1
        ,event = #'AstronomicalEvent'{
            body = 'Sun'
            ,atype = 'Rise'
            ,zenith = 'Official'
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
        "name" => "LightingAstronomicalEvent"
        ,"id" => 1
        ,"event" => #{
            "name" => "AstronomicalEvent"
            ,"body" => "Sun"
            ,"atype" => "Rise"
            ,"zenith" => "Official"
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
    ?assertEqual(LightingAstronomicalEvent, to_record(Map)),
    'ok'.

create_test() ->
    Event = 'astronomicalEvent':create('Sun', 'Rise', 'Official', 1, 1, 1, 2017),
    State = lightingCtrl:create(1, 55, 0, 12),
    ?assertMatch(#'LightingAstronomicalEvent'{
        id = 1
        ,event = #'AstronomicalEvent'{
            body = 'Sun'
            ,atype = 'Rise'
            ,zenith = 'Official'
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
