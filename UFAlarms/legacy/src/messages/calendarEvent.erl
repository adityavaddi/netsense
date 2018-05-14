%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Calendar Event ==
%% Calendar Event message API
%% @end
%%%-------------------------------------------------------------------
-module(calendarEvent).

-export([
    to_map/1
    ,to_record/1
    ,create/7
    ,key/2
]).

-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type 'CalendarEvent'() :: #'CalendarEvent'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert CalendarEvent record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('CalendarEvent'()) -> map().
to_map(CE) ->
    #{
        "name" => "CalendarEvent"
        ,"sec" => CE#'CalendarEvent'.sec
        ,"min" => CE#'CalendarEvent'.min
        ,"hr" => CE#'CalendarEvent'.hr
        ,"wday" => legacy_helpers:undef_atom_to_string(CE#'CalendarEvent'.wday)
        ,"mday" => legacy_helpers:undef_atom_to_string(CE#'CalendarEvent'.mday)
        ,"mon" => legacy_helpers:undef_atom_to_string(CE#'CalendarEvent'.mon)
        ,"year" => legacy_helpers:undef_atom_to_string(CE#'CalendarEvent'.year)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert CalendarEvent record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'CalendarEvent'().
to_record(Map) ->
    #'CalendarEvent'{
        sec = maps:get("sec", Map)
        ,min = maps:get("min", Map)
        ,hr = maps:get("hr", Map)
        ,wday = legacy_helpers:undef_string_to_atom(maps:get("wday", Map, 'undefined'))
        ,mday = legacy_helpers:undef_string_to_atom(maps:get("mday", Map, 'undefined'))
        ,mon = legacy_helpers:undef_string_to_atom(maps:get("mon", Map, 'undefined'))
        ,year = legacy_helpers:undef_string_to_atom(maps:get("year", Map, 'undefined'))
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(integer(), integer(), integer(), integer() | 'undefined'
             ,integer() | 'undefined', integer() | 'undefined'
             ,integer() | 'undefined') -> 'CalendarEvent'().
create(Sec, Min, Hr, Wday, Mday, Mon, Year) ->
    #'CalendarEvent'{
        sec = Sec
        ,min = Min
        ,hr = Hr
        ,wday = Wday
        ,mday = Mday
        ,mon = Mon
        ,year = Year
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
    CalendarEvent = #'CalendarEvent'{
        sec = 12
        ,min = 12
        ,hr = 11
        ,wday = 1
        ,mday = 1
        ,mon = 1
        ,year = 2017
    },
    Map = #{
        "name" => "CalendarEvent"
        ,"sec" => 12
        ,"min" => 12
        ,"hr" => 11
        ,"wday" => 1
        ,"mday" => 1
        ,"mon" => 1
        ,"year" => 2017
    },
    ?assertEqual(Map, to_map(CalendarEvent)),
    'ok'.

to_record_test() ->
    CalendarEvent = #'CalendarEvent'{
        sec = 12
        ,min = 12
        ,hr = 11
        ,wday = 1
        ,mday = 1
        ,mon = 1
        ,year = 2017
    },
    Map = #{
        "name" => "CalendarEvent"
        ,"sec" => 12
        ,"min" => 12
        ,"hr" => 11
        ,"wday" => 1
        ,"mday" => 1
        ,"mon" => 1
        ,"year" => 2017
    },
    ?assertEqual(CalendarEvent, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'CalendarEvent'{
        sec = 12
        ,min = 12
        ,hr = 11
        ,wday = 1
        ,mday = 1
        ,mon = 1
        ,year = 2017
    }, create(12, 12, 11, 1, 1, 1, 2017)).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
