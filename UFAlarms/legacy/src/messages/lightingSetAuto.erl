%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Lighting Set Auto ==
%% Lighting Set Auto message API
%% @end
%%%-------------------------------------------------------------------
-module(lightingSetAuto).

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

-type 'LightingSetAuto'() :: #'LightingSetAuto'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert LightingSetAuto record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('LightingSetAuto'()) -> map().
to_map(_UR) ->
    #{"name" => "LightingSetAuto"}.

%%--------------------------------------------------------------------
%% @doc
%% Convert LightingSetAuto record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'LightingSetAuto'().
to_record(_Map) ->
    #'LightingSetAuto'{}.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create() -> 'LightingSetAuto'().
create() ->
    #'LightingSetAuto'{}.

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
    LightingSetAuto = #'LightingSetAuto'{},
    Map = #{"name" => "LightingSetAuto"},
    ?assertEqual(Map, to_map(LightingSetAuto)),
    'ok'.

to_record_test() ->
    LightingSetAuto = #'LightingSetAuto'{},
    Map = #{"name" => "LightingSetAuto"},
    ?assertEqual(LightingSetAuto, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'LightingSetAuto'{}, create()).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
