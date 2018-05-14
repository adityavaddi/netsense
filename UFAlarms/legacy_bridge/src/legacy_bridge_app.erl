%% @author Shailen Karur
%% @copyright 2015 Sensity Inc.
%% @doc This module is the app that spawns multiple cowboy_websocket handlers for unodes and vnodes

-module(legacy_bridge_app).
-author("Gina Hagg ghagg@sensity.com").
-behaviour(application).

%% Application callbacks
-export([start/0, stop/0, get_app_env/2, set_app_env/2, unset_app_env/1]).
-export([start/2, start_phase/3, stop/1]).

-compile({inline, [set_app_env/2, unset_app_env/1]}).
%%-------------------------------------------------------------------
%% ADMIN API
%%-------------------------------------------------------------------
%% @doc Starts the application
%% @doc Starts the application
-spec start() -> ok | {error, {already_started, ?MODULE}}.
start() ->
lager:start(),
application:start(crypto),
application:start(public_key),
application:start(asn1),
application:start(ssl),
application:start(gproc),
application:set_env(mnesia, dc_dump_limit, 40),
application:set_env(mnesia, dump_log_write_threshold, 10000),
application:start(mnesia),
mnesia:wait_for_tables([legacy_bridge], 5000),
application:start(erlcron),
application:ensure_all_started(exometer),
application:start(?MODULE).

%% @doc Stops the application
-spec stop() -> ok.
stop() -> application:stop(?MODULE).

-spec get_app_env(atom(), any()) -> any().
get_app_env(Param, Default) ->
    case application:get_env(legacy_bridge, Param) of
        {ok, Val} -> Val;
        undefined -> Default
    end.

-spec set_app_env(atom(), any()) -> ok.
set_app_env(Param, Val) ->
    application:set_env(legacy_bridge, Param, Val).

-spec unset_app_env(atom()) -> ok.
unset_app_env(Param) ->
    application:unset_env(legacy_bridge, Param).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(atom(), any()) -> {ok, pid()}.
-spec start_phase(atom(), atom(), any()) -> ok.
-spec stop(any()) -> ok.

start(_StartType, _StartArgs) ->
    legacy_bridge_sup:start_link().
    %gen_event:add_handler(send_light_schedule, lighting_scheduler, []).

start_phase(join_cluster = _Phase, _StartType, _PhaseArgs) ->
    ok;
start_phase(listen = _Phase, _, _) ->
    legacy_bridge_sup:start_cowboy_listeners(get_app_env(listener_count, 100)).

stop(_State) ->
    ok.
