-module(dcc_pool_sup).
-author("Gina Hagg ghagg@sensity.com").
-behaviour(supervisor).

-export([start_link/0, stop/0]).
-export([start/2, stop/1]).
-export([init/1]).
-compile([{parse_transform, lager_transform}]).

start_link() ->
    lager:debug("dcc_pool_sup starting"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    application:stop(?MODULE).

start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

init(_Args) ->
    {ok, Pools} = application:get_env(device_service, pools),
    PoolSpecss = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        lager:debug("****************************************************************Name is ~n  ~p ~n ", [Name]),
        case Name of dcc_zmq_pool ->
            ZmqCArgs = [{name, {local, Name}},
                    {worker_module, dcc_zmq_client}] ++ SizeArgs,
            poolboy:child_spec(Name, ZmqCArgs, WorkerArgs);
           dcc_zmq_listener_pool ->
            ListenerArgs = [{name, {local, Name}},
                    {worker_module, dcc_zmq_listener}] ++ SizeArgs,
            poolboy:child_spec(Name, ListenerArgs, WorkerArgs)
        end
    end, Pools),
    PoolSpecs = helpers:flatten3(PoolSpecss),
    lager:debug("****POOLSPECS:*********> ~p~n",[PoolSpecs]),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.