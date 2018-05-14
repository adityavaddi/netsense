%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @reference <a href="https://xeranet.atlassian.net/wiki/display/NSN/Firmware+OTA" target="_blank">OTA SPEC</a>
%% @doc
%% == Legacy OTA Supervisor ==
%% Spec: `{one_for_one, 10, 10}' child spawned as `transient'
%% @end
%%%-------------------------------------------------------------------
-module(legacy_ota_sup).

-behaviour(supervisor).

-include("legacy.hrl").

%% API
-export([
    start_link/0
    ,handle/1
    ,terminate_child/1
]).

%% Supervisor callbacks
-export([init/1]).

-define(WORKER(ID, Mod, Args), {ID, {Mod, 'start_link', Args}, 'transient', 5000, 'worker', [Mod]}).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts a `legacy_ota_scheduler' with given config
%% @end
%%--------------------------------------------------------------------
-spec handle(map()) -> any().
handle(Args) ->
    {'ok', OTAConfig} = legacy_config:get('ota'),
    ID = maps:get("jobid", Args),
    lager:info("creating child with ~p", [[OTAConfig, Args, ID]]),
    {'ok', _Child} = supervisor:start_child(?MODULE, ?WORKER(ID, 'legacy_ota_scheduler', [[OTAConfig, Args, ID]])).

%%--------------------------------------------------------------------
%% @doc
%% Terminate Children
%% @end
%%--------------------------------------------------------------------
-spec terminate_child(string()) -> any().
terminate_child(ID) ->
    erlang:spawn(
        fun() ->
            timer:sleep(10),
            supervisor:delete_child(?MODULE, ID)
        end
    ).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init(_) ->
    lager:info("init"),
    {'ok', { {'one_for_one', 10, 10}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
