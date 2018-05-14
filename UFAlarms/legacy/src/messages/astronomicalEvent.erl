%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Calendar Event ==
%% Calendar Event message API
%% @end
%%%-------------------------------------------------------------------
-module(astronomicalEvent).

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

-type 'AstronomicalEvent'() :: #'AstronomicalEvent'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert AstronomicalEvent record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('AstronomicalEvent'()) -> map().
to_map(CE) ->
    #{
        "name" => "AstronomicalEvent"
        ,"body" => legacy_helpers:symbol_to_string(CE#'AstronomicalEvent'.body)
        ,"atype" => legacy_helpers:symbol_to_string(CE#'AstronomicalEvent'.atype)
        ,"zenith" => legacy_helpers:symbol_to_string(CE#'AstronomicalEvent'.zenith)
        ,"wday" => legacy_helpers:undef_atom_to_string(CE#'AstronomicalEvent'.wday)
        ,"mday" => legacy_helpers:undef_atom_to_string(CE#'AstronomicalEvent'.mday)
        ,"mon" => legacy_helpers:undef_atom_to_string(CE#'AstronomicalEvent'.mon)
        ,"year" => legacy_helpers:undef_atom_to_string(CE#'AstronomicalEvent'.year)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert AstronomicalEvent record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'AstronomicalEvent'().
to_record(Map) ->
    #'AstronomicalEvent'{
        body = legacy_helpers:symbol_to_atom(maps:get("body", Map))
        ,atype = legacy_helpers:symbol_to_atom(maps:get("atype", Map))
        ,zenith = legacy_helpers:symbol_to_atom(maps:get("zenith", Map))
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
-spec create(atom(), atom(), atom(), integer() | 'undefined'
             ,integer() | 'undefined', integer() | 'undefined'
             ,integer() | 'undefined') -> 'AstronomicalEvent'().
create(Body, AType, Zenith, Wday, Mday, Mon, Year) ->
    #'AstronomicalEvent'{
        body = Body
        ,atype = AType
        ,zenith = Zenith
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
    AstronomicalEvent = #'AstronomicalEvent'{
        body = 'Sun'
        ,atype = 'Rise'
        ,zenith = 'Official'
        ,wday = 1
        ,mday = 1
        ,mon = 1
        ,year = 2017
    },
    Map = #{
        "name" => "AstronomicalEvent"
        ,"body" => "Sun"
        ,"atype" => "Rise"
        ,"zenith" => "Official"
        ,"wday" => 1
        ,"mday" => 1
        ,"mon" => 1
        ,"year" => 2017
    },
    ?assertEqual(Map, to_map(AstronomicalEvent)),
    'ok'.

to_record_test() ->
    AstronomicalEvent = #'AstronomicalEvent'{
        body = 'Sun'
        ,atype = 'Rise'
        ,zenith = 'Official'
        ,wday = 1
        ,mday = 1
        ,mon = 1
        ,year = 2017
    },
    Map = #{
        "name" => "AstronomicalEvent"
        ,"body" => "Sun"
        ,"atype" => "Rise"
        ,"zenith" => "Official"
        ,"wday" => 1
        ,"mday" => 1
        ,"mon" => 1
        ,"year" => 2017
    },
    ?assertEqual(AstronomicalEvent, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'AstronomicalEvent'{
        body = 'Sun'
        ,atype = 'Rise'
        ,zenith = 'Official'
        ,wday = 1
        ,mday = 1
        ,mon = 1
        ,year = 2017
    }, create('Sun', 'Rise', 'Official', 1, 1, 1, 2017)).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
