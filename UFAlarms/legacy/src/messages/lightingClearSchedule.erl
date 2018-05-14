%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Lighting Clear Schedule ==
%% Lighting Clear Schedule message API
%% @end
%%%-------------------------------------------------------------------
-module(lightingClearSchedule).

-export([
    to_map/1
    ,to_record/1
    ,create/0
    ,key/2
]).

-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type 'LightingClearSchedule'() :: #'LightingClearSchedule'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert LightingClearSchedule record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('LightingClearSchedule'()) -> map().
to_map(_UR) ->
    #{"name" => "LightingClearSchedule"}.

%%--------------------------------------------------------------------
%% @doc
%% Convert LightingClearSchedule record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'LightingClearSchedule'().
to_record(_Map) ->
    #'LightingClearSchedule'{}.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create() -> 'LightingClearSchedule'().
create() ->
    #'LightingClearSchedule'{}.

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
    LightingClearSchedule = #'LightingClearSchedule'{},
    Map = #{"name" => "LightingClearSchedule"},
    ?assertEqual(Map, to_map(LightingClearSchedule)),
    'ok'.

to_record_test() ->
    LightingClearSchedule = #'LightingClearSchedule'{},
    Map = #{"name" => "LightingClearSchedule"},
    ?assertEqual(LightingClearSchedule, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'LightingClearSchedule'{}, create()).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
