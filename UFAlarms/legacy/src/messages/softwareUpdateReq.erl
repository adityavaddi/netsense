%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Software Update Request ==
%% Software Update Request message API
%% @end
%%%-------------------------------------------------------------------
-module(softwareUpdateReq).

-export([
    to_map/1
    ,to_record/1
    ,create/2
    ,key/2
]).

-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type 'SoftwareUpdateReq'() :: #'SoftwareUpdateReq'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert SoftwareUpdateReq record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('SoftwareUpdateReq'()) -> map().
to_map(SUR) ->
    #{
        "name" => "SoftwareUpdateReq"
        ,"swUpdateURL" =>  legacy_helpers:symbol_to_string(SUR#'SoftwareUpdateReq'.swUpdateURL)
        ,"cellFwUpdateURL" =>  legacy_helpers:symbol_to_string(SUR#'SoftwareUpdateReq'.cellFwUpdateURL)
        ,"has_cellFwUpdateURL" =>  SUR#'SoftwareUpdateReq'.has_cellFwUpdateURL
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert SoftwareUpdateReq record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'SoftwareUpdateReq'().
to_record(Map) ->
    #'SoftwareUpdateReq'{
        swUpdateURL = legacy_helpers:symbol_to_binary(maps:get("swUpdateURL", Map, 'undefined'), 'undefined')
        ,cellFwUpdateURL = legacy_helpers:symbol_to_binary(maps:get("cellFwUpdateURL", Map, 'undefined'), 'undefined')
        ,has_cellFwUpdateURL = maps:get("has_cellFwUpdateURL", Map, 'undefined')
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(binary(), binary()) -> 'SoftwareUpdateReq'().
create(SwUpdateURL, 'undefined') ->
    #'SoftwareUpdateReq'{
        swUpdateURL=SwUpdateURL
        ,cellFwUpdateURL='undefined'
        ,has_cellFwUpdateURL='false'
    };
create(SwUpdateURL, CellFwUpdateURL) ->
    #'SoftwareUpdateReq'{
        swUpdateURL=SwUpdateURL
        ,cellFwUpdateURL=CellFwUpdateURL
        ,has_cellFwUpdateURL='true'
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
    SoftwareUpdateReq = #'SoftwareUpdateReq'{
        swUpdateURL= <<"http://127.0.0.1/ota/J007.bin">>
        ,cellFwUpdateURL= <<"http://127.0.0.1/ota/J007.zip">>
        ,has_cellFwUpdateURL='true'
    },
    Map = #{
        "name" => "SoftwareUpdateReq"
        ,"swUpdateURL" => "http://127.0.0.1/ota/J007.bin"
        ,"cellFwUpdateURL" => "http://127.0.0.1/ota/J007.zip"
        ,"has_cellFwUpdateURL" => 'true'
    },
    ?assertEqual(Map, to_map(SoftwareUpdateReq)),
    'ok'.

to_record_test() ->
    SoftwareUpdateReq = #'SoftwareUpdateReq'{
        swUpdateURL= <<"http://127.0.0.1/ota/J007.bin">>
        ,cellFwUpdateURL= <<"http://127.0.0.1/ota/J007.zip">>
        ,has_cellFwUpdateURL='true'
    },
    Map = #{
        "name" => "SoftwareUpdateReq"
        ,"swUpdateURL" => "http://127.0.0.1/ota/J007.bin"
        ,"cellFwUpdateURL" => "http://127.0.0.1/ota/J007.zip"
        ,"has_cellFwUpdateURL" => 'true'
    },
    ?assertEqual(SoftwareUpdateReq, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'SoftwareUpdateReq'{
        swUpdateURL= <<"http://127.0.0.1/ota/J007.bin">>
        ,cellFwUpdateURL= <<"http://127.0.0.1/ota/J007.zip">>
        ,has_cellFwUpdateURL='true'
    }, create(<<"http://127.0.0.1/ota/J007.bin">>, <<"http://127.0.0.1/ota/J007.zip">>)).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
