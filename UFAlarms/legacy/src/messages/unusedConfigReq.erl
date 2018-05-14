%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Unused Config Request ==
%% Unused Config Request message API
%% @end
%%%-------------------------------------------------------------------
-module(unusedConfigReq).

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

-type 'UnusedConfigReq'() :: #'UnusedConfigReq'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert UnusedConfigReq record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('UnusedConfigReq'()) -> map().
to_map(_UR) ->
    #{"name" => "UnusedConfigReq"}.

%%--------------------------------------------------------------------
%% @doc
%% Convert UnusedConfigReq record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'UnusedConfigReq'().
to_record(_Map) ->
    #'UnusedConfigReq'{}.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create() -> 'UnusedConfigReq'().
create() ->
    #'UnusedConfigReq'{}.

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
    UnusedConfigReq = #'UnusedConfigReq'{},
    Map = #{"name" => "UnusedConfigReq"},
    ?assertEqual(Map, to_map(UnusedConfigReq)),
    'ok'.

to_record_test() ->
    UnusedConfigReq = #'UnusedConfigReq'{},
    Map = #{"name" => "UnusedConfigReq"},
    ?assertEqual(UnusedConfigReq, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'UnusedConfigReq'{}, create()).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
