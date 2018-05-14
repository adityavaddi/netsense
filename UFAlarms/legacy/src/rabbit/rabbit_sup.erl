%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == RabbitMQ Top Supervisor ==
%% === Starting: ===
%% <ul>
%% <li><a href="rabbit_conn_worker.html">rabbit_conn_worker</a></li>
%% <li><a href="rabbit_pool_worker.html">rabbit_pool_worker(s)</a></li>
%% </ul>
%% Spec: rest_for_one 10 10
%% @end
%%%-------------------------------------------------------------------
-module(rabbit_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(WORKER(ID, Mod, Args), {ID, {Mod, 'start_link', Args}, 'permanent', 5000, 'worker', [Mod]}).
-define(SUP(I, Args), {I, {I, 'start_link', Args}, 'permanent', 5000, 'supervisor', [I]}).

%%====================================================================
%% API functions
%%====================================================================
start_link(Args) ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init(Args) ->
    lager:info("rabbitmq config: ~p", [Args]),

    ConnWorker = ?WORKER('rabbit_conn_worker', 'rabbit_conn_worker', [Args]),
    PublishLB = ?WORKER('rabbit_conn_watcher', 'rabbit_conn_watcher', [Args]),

    Specs = [ConnWorker, PublishLB],
    {'ok', {{'rest_for_one', 10, 10}, Specs}}.

%%====================================================================
%% Internal functions
%%====================================================================
