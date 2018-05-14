%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Legacy Top Supervisor ==
%% === Starting: ===
%% <ul>
%% <li><a href="rabbit_sup.html">rabbit_sup</a></li>
%% <li><a href="legacy_s3_worker.html">legacy_s3_worker</a></li>
%% <li><a href="legacy_ota_sup.html">legacy_ota_sup</a></li>
%% <li><a href="legacy_listeners.html">legacy_listeners</a></li>
%% <li><a href="legacy_metrics.html">legacy_metrics</a></li>
%% </ul>
%% <p>
%% It will start childs in this order and shut then down in reverse.
%% This will cause http(s) listeners to go down first allowing them
%% to send disconnect messages to rabbit.
%% </p>
%% Spec: one_for_one 10 10
%% @end
%%%-------------------------------------------------------------------
-module(legacy_sup).

-behaviour(supervisor).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("legacy.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


-define(WORKER(I, Args), {I, {I, 'start_link', Args}, 'permanent', 5000, 'worker', [I]}).
-define(SUP(I, Args), {I, {I, 'start_link', Args}, 'permanent', 5000, 'supervisor', [I]}).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init(_) ->
    {'ok', RabbitCfg} = legacy_config:get('rabbitmq'),
    {'ok', S3Config} = legacy_config:get('s3'),
    {'ok', MetricsConfig} = legacy_config:get('metrics'),

    Specs = [
        ?SUP('rabbit_sup', [RabbitCfg])
        ,?WORKER('legacy_s3_worker', [S3Config])
        ,?SUP('legacy_ota_sup', [])
        ,?WORKER('legacy_listeners', [])
        ,?WORKER('legacy_metrics', [MetricsConfig])
    ],
    {'ok', { {'one_for_one', 10, 10}, Specs} }.

%%====================================================================
%% Internal functions
%%====================================================================
