-module(legacy_bridge_sup).
-author('skarur@sensity.com').
-author("Gina Hagg ghagg@sensity.com").
-vsn('').

-behaviour(supervisor).

%%-include("en.hrl").
%% API
-export([start_link/0, start_cowboy_listeners/1, stop_cowboy_listeners/0,
         set_service_on/0, connect_nodes/0]).

-type restart() :: {supervisor:strategy(), non_neg_integer(), non_neg_integer()}.
-type sup_init_return() :: {ok, {restart(), [supervisor:child_spec()]}}.

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    timer:apply_after(500, ?MODULE, set_service_on, []),
    {ok, Pid}.

-spec stop_cowboy_listeners() -> ok | {missing, cowboy_sup}.
stop_cowboy_listeners() ->
    case whereis(cowboy_sup) of
        undefined -> {missing, cowboy_sup};
        _Pid -> cowboy:stop_listener(legacy_bridge_http)
    end.

-spec start_cowboy_listeners(non_neg_integer()) -> ok | {missing, cowboy_sup}.
start_cowboy_listeners(NumListeners) ->
    case whereis(cowboy_sup) of
        undefined -> {missing, cowboy_sup};
        _Pid ->
            start_listeners(NumListeners)
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec connect_nodes() -> ok.


start_listeners(NumListeners) ->
    Static_Opts = [
                   {directory, {priv_dir, legacy_bridge, []}},
                   {mimetypes, [
                                {<<".css">>,   [<<"text/css">>]},
                                {<<".js">>,    [<<"application/javascript">>]},
                                {<<".json">>,  [<<"application/json">>]},
                                {<<".html">>,  [<<"text/html">>]}
                               ]}
                   ],

    Dispatch =
        cowboy_router:compile(
            [{'_', [
                     {<<"/websocket">>, legacy_ws_handler, []},
                     {<<"/device/:node_id/ws/push">>, legacy_ws_handler, []},
                     {<<"/device/:node_id">>, legacy_ws_handler, []},
                     {<<"/device/:node_id/:token/cert.clkey">>, 'firmware_handler', []},
                     {<<"/device/:node_id/:fwid/:file">>, 'firmware_handler', []},
                     {<<"/device/:node_id/fw/:fwid">>, 'firmware_handler', []},
                     {<<"/nodelist">>, 'mynodes_handler', []},
                     %{"/nodes/[...]", cowboy_static, {priv_dir, legacy_bridge, "static/nodes"}},
                     {"/nodes", cowboy_static, {priv_file, legacy_bridge, "static/mynodes.html"}},
                     {"/", cowboy_static, {priv_file, legacy_bridge, "static/index.html"}},
                     {<<"/...">>, cowboy_http_static, Static_Opts}
                   ]
             }]),

                     %% Dev API documentation.
                     %%{<<"/...">>, cowboy_http_static, Static_Opts}
                 %%]
                %%}]),

    Listener_Count  = legacy_bridge_app:get_app_env(listener_count,  NumListeners),

    {Http_Ranch, Http_Cowboy} = config_data(http, Dispatch),
    {ok, _} = cowboy:start_http(http, Listener_Count, Http_Ranch, Http_Cowboy),

    {Https_Ranch, Https_Cowboy} = config_data(https, Dispatch),
    {ok, _} = cowboy:start_https(https, Listener_Count, Https_Ranch, Https_Cowboy),

    ok.

set_service_on() ->
    true.

connect_nodes() ->
%%    case presence_client:online_server_procs(?EN) of
 %%       [] -> timer:apply_after(500, ?MODULE, connect_nodes, []), ok;
%%        _Procs -> ok
%%    end.
      ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init({}) -> sup_init_return().

-define(SUPER(__Mod, __Args, __Timeout), {__Mod, {__Mod, start_link, __Args}, permanent, __Timeout, supervisor, [__Mod]}).
%-define(CHILD(__Mod, __Args, __Timeout), {__Mod, {__Mod, start_link, __Args}, permanent, __Timeout, worker, [__Mod]}).

init([]) ->
    start_listeners(legacy_bridge_app:get_app_env(listener_count, 100)),
    DeadCollector = ?CHILD(dead_node_collector,worker),
    S3Server = ?CHILD(legacy_s3_server, worker),
    LegacyDB = ?CHILD(legacy_db_server, worker),
    LSubscriber = ?CHILD(legacy_mqtt_agent,worker),
    PoolSup = ?CHILD(legacy_pool_sup, supervisor),
    ZMQPullListener = ?CHILD(zmqpulllistener,worker),
    ZMQPushClient = ?CHILD(zmqpushclient,worker),
    {ok, { {one_for_one, 10, 10}, [ZMQPullListener, ZMQPushClient, S3Server , DeadCollector,
    LSubscriber, LegacyDB,
    PoolSup
    ]}}.


%% Utilities
config_data(http, Dispatch) ->
    Port            = legacy_bridge_app:get_app_env(listen_port,     8080),
    Max_Connections = legacy_bridge_app:get_app_env(max_connections, 1000),
    Timeout         = legacy_bridge_app:get_app_env(http_timeout,      60),
    RanchOptions    = [{port, Port}, {max_connections, Max_Connections},{keepalive, true}],
    CowboyOptions   = [{env, [{dispatch, Dispatch}]}, {compress, false}, {timeout, Timeout * 1000}],%%
                       %%{onresponse, fun legacy_bridge_response:response_hook/4}],
    {RanchOptions, CowboyOptions};
config_data(https, Dispatch) ->
    DefaultCertFile = filename:join(code:priv_dir(legacy_bridge), "ssl/old/server.pem"),
    DefaultKeyFile  = filename:join(code:priv_dir(legacy_bridge), "ssl/old/server-key.pem"),
    DefaultCaCertFile  = filename:join(code:priv_dir(legacy_bridge), "ssl/old/sensity.crt"),
    %SSLDir = code:lib_dir(en, priv) ++ "/ssl/",
    %{ok, _} = cowboy:start_https(https, 100, [{port, 10443},
    %                                          {cacertfile, SSLDir ++ "sensity.crt"},
    %                                          {certfile, SSLDir ++ "server.pem"},
    %                                          {keyfile, SSLDir ++ "server-key.pem"}],
    %                             [{env, [{dispatch, Dispatch}]}]),
    DefaultCiphers  = [CS || CS = {X, _, _} <- ssl:cipher_suites(), X /= ecdhe_rsa],

    Port            = legacy_bridge_app:get_app_env(https_port,      10443),
    Max_Connections = legacy_bridge_app:get_app_env(https_max_conns, 1000),
    CertFile        = legacy_bridge_app:get_app_env(https_certfile,  DefaultCertFile),
    CaCertFile      = legacy_bridge_app:get_app_env(https_cacertfile,  DefaultCaCertFile),
    KeyFile         = legacy_bridge_app:get_app_env(https_keyfile,   DefaultKeyFile),
    Ciphers         = legacy_bridge_app:get_app_env(https_ciphers,   DefaultCiphers),

    RanchOptions =
        [   {port, Port}
        ,   {max_connections, Max_Connections}
        ,   {keepalive, true}
        ,   {cacertfile, CaCertFile}
        ,   {certfile, CertFile}
        ,   {keyfile, KeyFile}
        ,   {ciphers, Ciphers}],
    CowboyOptions = [{env, [{dispatch, Dispatch}]}, {compress, false}],
    {RanchOptions, CowboyOptions}.
