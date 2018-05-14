-module(device_service_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	%Procs = [{commandcenter, {commandcenter, start_link, []}, permanent, 2000, worker, [commandcenter]}],
	CommandCenter = ?CHILD(commandcenter, worker, []),
	ZMQPushClient = ?CHILD(zmqpushclient, worker, []),
	DCC_CACHE = ?CHILD(dcc_cache, worker, []),
	{'ok', MetricsConfig} = application:get_env('device_service', 'metrics'),
	Metrics = ?CHILD('dcc_metrics', 'worker', [MetricsConfig]),
	{ok, {{one_for_one, 10, 10}, [CommandCenter, ZMQPushClient, DCC_CACHE, Metrics]}}.
