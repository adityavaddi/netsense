%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Login Response ==
%% Login Response message API
%% @end
%%%-------------------------------------------------------------------
-module(loginResp).

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

-type 'LoginResp'() :: #'LoginResp'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert LoginResp record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('LoginResp'()) -> map().
to_map(LR) ->
    #{
        "name" => "LoginResp"
        ,"okay" => LR#'LoginResp'.okay
        ,"time" =>  legacy_helpers:undef_atom_to_string(LR#'LoginResp'.time)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert LoginResp record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'LoginResp'().
to_record(Map) ->
    #'LoginResp'{
        okay = maps:get("okay", Map)
        ,time = maps:get("time", Map)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(integer()) -> 'LoginResp'().
create(Time) ->
    #'LoginResp'{
        okay = 'true'
        ,time = Time
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
    LoginResp = #'LoginResp'{
        okay='true'
        ,time=0
    },
    Map = #{
        "name" => "LoginResp"
        ,"okay" => 'true'
        ,"time" => 0
    },
    ?assertEqual(Map, to_map(LoginResp)),
    'ok'.

to_record_test() ->
    LoginResp = #'LoginResp'{
        okay='true'
        ,time=0
    },
    Map = #{
        "name" => "LoginResp"
        ,"okay" => 'true'
        ,"time" => 0
    },
    ?assertEqual(LoginResp, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'LoginResp'{okay='true', time=1}, create(1)).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
