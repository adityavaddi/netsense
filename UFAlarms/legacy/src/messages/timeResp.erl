%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Time Response ==
%% Time Response message API
%% @end
%%%-------------------------------------------------------------------
-module(timeResp).

-export([
    to_map/1
    ,to_record/1
    ,create/1
    ,key/2
]).

-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type 'TimeResp'() :: #'TimeResp'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert TimeResp record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('TimeResp'()) -> map().
to_map(TR) ->
    #{
        "name" => "TimeResp"
        ,"time" => TR#'TimeResp'.time
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert TimeResp record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'TimeResp'().
to_record(Map) ->
    #'TimeResp'{
        time = maps:get("time", Map)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(integer()) -> 'TimeResp'().
create(Time) ->
    #'TimeResp'{time=Time}.

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
    TimeResp = #'TimeResp'{time = 0},
    Map = #{"name" => "TimeResp", "time" => 0},
    ?assertEqual(Map, to_map(TimeResp)),
    'ok'.

to_record_test() ->
    TimeResp = #'TimeResp'{time = 0},
    Map = #{"name" => "TimeResp", "time" => 0},
    ?assertEqual(TimeResp, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'TimeResp'{time=0}, create(0)).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
