%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Certificate Update ==
%% Certificate Update message API
%% @end
%%%-------------------------------------------------------------------
-module(x509UpdateReq).

-export([
    to_map/1
    ,to_record/1
    ,create/3
    ,key/2
]).

-include("messages_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type 'X509UpdateReq'() :: #'X509UpdateReq'{}.

%%--------------------------------------------------------------------
%% @doc
%% Convert X509UpdateReq record to map
%% @end
%%--------------------------------------------------------------------
-spec to_map('X509UpdateReq'()) -> map().
to_map(X509UR) ->
    #{
        "name" => "X509UpdateReq"
        ,"x509UpdateURL" => legacy_helpers:symbol_to_string(X509UR#'X509UpdateReq'.x509UpdateURL)
        ,"x509Update" => legacy_helpers:symbol_to_string(X509UR#'X509UpdateReq'.x509Update)
        ,"httpsPort" => legacy_helpers:symbol_to_string(X509UR#'X509UpdateReq'.httpsPort)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Convert X509UpdateReq record to map
%% @end
%%--------------------------------------------------------------------
-spec to_record(map()) -> 'X509UpdateReq'().
to_record(Map) ->
    #'X509UpdateReq'{
        x509UpdateURL = legacy_helpers:symbol_to_binary(maps:get("x509UpdateURL", Map))
        ,x509Update = legacy_helpers:symbol_to_atom(maps:get("x509Update", Map))
        ,httpsPort = legacy_helpers:undef_string_to_atom(maps:get("httpsPort", Map, 'undefined'))
    }.

%%--------------------------------------------------------------------
%% @doc
%% Create login response
%% @end
%%--------------------------------------------------------------------
-spec create(binary(), atom(), integer() | 'undefined') -> 'X509UpdateReq'().
create(Url, Update, Port) ->
    #'X509UpdateReq'{
        x509UpdateURL = Url
        ,x509Update = Update
        ,httpsPort = Port
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
    X509UpdateReq = #'X509UpdateReq'{
        x509UpdateURL = <<"http://127.0.0.1:10443/certs">>
        ,x509Update = 'X509Both'
        ,httpsPort = 10443
    },
    Map = #{
        "name" => "X509UpdateReq"
        ,"x509UpdateURL" => "http://127.0.0.1:10443/certs"
        ,"x509Update" => "X509Both"
        ,"httpsPort" => 10443
    },
    ?assertEqual(Map, to_map(X509UpdateReq)),
    'ok'.

to_record_test() ->
    X509UpdateReq = #'X509UpdateReq'{
        x509UpdateURL = <<"http://127.0.0.1:10443/certs">>
        ,x509Update = 'X509Both'
        ,httpsPort = 10443
    },
    Map = #{
        "name" => "X509UpdateReq"
        ,"x509UpdateURL" => "http://127.0.0.1:10443/certs"
        ,"x509Update" => "X509Both"
        ,"httpsPort" => 10443
    },
    ?assertEqual(X509UpdateReq, to_record(Map)),
    'ok'.

create_test() ->
    ?assertMatch(#'X509UpdateReq'{
        x509UpdateURL = <<"http://127.0.0.1:10443/certs">>
        ,x509Update = 'X509Both'
        ,httpsPort = 10443
    }, create(<<"http://127.0.0.1:10443/certs">>, 'X509Both', 10443)).

key_test() ->
    ?assertEqual(<<>>, key(<<>>, "stuff")).

-endif.
