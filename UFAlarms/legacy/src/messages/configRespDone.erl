%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Config Response Done ==
%% Config Response Done message API
%% @end
%%%-------------------------------------------------------------------
-module(configRespDone).

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

-type 'ConfigRespDone'() :: #'ConfigRespDone'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert ConfigRespDone record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('ConfigRespDone'()) -> map().
to_map(_UR) ->
    #{"name" => "ConfigRespDone"}.

%%--------------------------------------------------------------------
%% @doc
%% Convert ConfigRespDone record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'ConfigRespDone'().
to_record(_Map) ->
    #'ConfigRespDone'{}.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create() -> 'ConfigRespDone'().
create() ->
    #'ConfigRespDone'{}.

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
    ConfigRespDone = #'ConfigRespDone'{},
    Map = #{"name" => "ConfigRespDone"},
    ?assertEqual(Map, to_map(ConfigRespDone)),
    'ok'.

to_record_test() ->
    ConfigRespDone = #'ConfigRespDone'{},
    Map = #{"name" => "ConfigRespDone"},
    ?assertEqual(ConfigRespDone, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'ConfigRespDone'{}, create()).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
