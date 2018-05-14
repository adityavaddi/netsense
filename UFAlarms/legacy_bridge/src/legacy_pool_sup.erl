-module(legacy_pool_sup).
-author("Gina Hagg ghagg@sensity.com").
-behaviour(supervisor).

-export([start_link/0, stop/0]).
-export([start/2, stop/1]).
-export([init/1]).
-compile([{parse_transform, lager_transform}]).

start_link() ->
    lager:debug("legacy_pool_sup starting~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    application:stop(?MODULE).

start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

init(_Args) ->
    {ok, Pools} = application:get_env(legacy_bridge, pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        lager:debug("****************************************************************Name is ~n  ~p ~n ", [Name]),
        case Name of
            distributor_pool ->
                 DistArgs = [{name, {local, Name}},
                        {worker_module, command_distributor}] ++ SizeArgs,
                poolboy:child_spec(Name, DistArgs, WorkerArgs)
        end            
    end, Pools),
    lager:debug("****POOLSPECS:*********> ~p~n",[PoolSpecs]),
    {ok, {{one_for_one, 10, 1000}, PoolSpecs}}.