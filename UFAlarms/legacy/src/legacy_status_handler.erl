%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Legacy Status handler ==
%% Return legacy status
%% @end
%%%-------------------------------------------------------------------
-module(legacy_status_handler).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    init/2
    ,terminate/3
]).

-include("legacy.hrl").
-include_lib("kernel/include/file.hrl").

-record(state, {}).

-type state() :: #state{}.
-type req() :: cowboy_req:req().

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Init request
%% @end
%%--------------------------------------------------------------------
-spec init(req(), list()) -> {'ok', req(), state()}.
init(Req0, _Opts) ->
    Path = cowboy_req:path(Req0),
    Content = content(Path),
    Req1 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(Content), Req0),
    lager:debug("rcvd request for ~p, responding with ~p", [Path, Content]),
    {ok, Req1, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% Called when request ternimated
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), req(), state()) -> 'ok'.
terminate(_Reason, _Req, _State) ->
    'ok'.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec content(binary()) -> map().
content(<<"/status/nodes/", Node/binary>>) ->
    case legacy_helpers:node_online(Node) of
        {'true', Pid} ->
            #{
                'name' => erlang:node()
                ,'connected' => 'true'
                ,'connected_to' => erlang:node(Pid)
            };
        'false' ->
            #{
                'name' => erlang:node()
                ,'connected' => 'false'
            }
    end;
content(<<"/status/nodes">>) ->
    GolbalNodes = legacy_metrics:correct_node_count(),
    lists:foldl(
        fun({Node, ErlNode}, Acc) ->
            M1 = maps:get(ErlNode, Acc, #{}),
            Count = maps:get('count', M1, 0),
            Nodes = maps:get('nodes', M1, []),
            M2 = #{
                'count' => Count + 1,
                'nodes' => [Node | Nodes]
            },
            maps:put(ErlNode, M2, Acc)
        end
        ,#{
            'name' => erlang:node()
            ,'cluster_count' => erlang:length(GolbalNodes)
        }
        ,GolbalNodes
    );
content(_) ->
    #{
        'name' => erlang:node()
        ,'cluster' => [erlang:node() | erlang:nodes()]
    }.
