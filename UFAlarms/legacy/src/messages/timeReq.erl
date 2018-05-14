%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Time Request ==
%% Time Request message API
%% @end
%%%-------------------------------------------------------------------
-module(timeReq).

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

-type 'TimeReq'() :: #'TimeReq'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert TimeReq record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('TimeReq'()) -> map().
to_map(_UR) ->
    #{"name" => "TimeReq"}.

%%--------------------------------------------------------------------
%% @doc
%% Convert TimeReq record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'TimeReq'().
to_record(_Map) ->
    #'TimeReq'{}.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create() -> 'TimeReq'().
create() ->
    #'TimeReq'{}.

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
    TimeReq = #'TimeReq'{},
    Map = #{"name" => "TimeReq"},
    ?assertEqual(Map, to_map(TimeReq)),
    'ok'.

to_record_test() ->
    TimeReq = #'TimeReq'{},
    Map = #{"name" => "TimeReq"},
    ?assertEqual(TimeReq, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'TimeReq'{}, create()).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
