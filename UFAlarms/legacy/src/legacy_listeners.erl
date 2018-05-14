%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Legacy listeners ==
%% Start and stop cowboy listeners for proper applicaiton start/stop
%% @end
%%%-------------------------------------------------------------------
-module(legacy_listeners).

-behavior(gen_server).

-include("legacy.hrl").

-define(HTTP_LISTENER, 'legacy_http').
-define(HTTPS_LISTENER, 'legacy_https').

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    start_link/0
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([
    init/1
    ,handle_call/3
    ,handle_cast/2
    ,handle_info/2
    ,terminate/2
    ,code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
    feed
    ,connected
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?SERVER, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(_Args) ->
    process_flag('trap_exit', 'true'),
    'ok' = rabbit_conn_worker:join_feed(self()),
    {'ok', #state{connected='false'}}.

handle_call(_Msg, _From, State) ->
    lager:warning("rcvd unknown call msg: ~p from: ~p", [_Msg, _From]),
    {'reply', 'ok', State}.

handle_cast(_Msg, State) ->
    lager:warning("rcvd unknown cast msg: ~p", [_Msg]),
    {'noreply', State}.

handle_info({'rabbit_conn_worker', {'joined', FeedID}}, State) ->
    lager:info("subscribed to rabbit_conn_worker"),
    {'noreply', State#state{feed=FeedID}};
handle_info({'rabbit_feed', {'ok', 'connected'}}, #state{connected='false'}=State) ->
    lager:info("rabbit connection connected"),
    _ = start_listeners(),
    {'noreply', State#state{connected='true'}};
handle_info({'rabbit_feed', {'ok', 'connected'}}, State) ->
    lager:debug("listeners already started"),
    {'noreply', State};
handle_info({'rabbit_feed', {'error', Status, Reason}}, #state{connected='true'}=State) ->
    lager:warning("rabbit connection: ~p ~p", [Status, Reason]),
    _ = stop_listeners(),
    {'noreply', State#state{connected='false'}};
handle_info({'rabbit_feed', {'error', Status, Reason}}, #state{connected='false'}=State) ->
    lager:warning("rabbit connection: ~p ~p", [Status, Reason]),
    {'noreply', State};
handle_info({'rabbit_feed', 'undefined'}, State) ->
    lager:debug("rabbit connection: undefined"),
    {'noreply', State};
handle_info(_Msg, State) ->
    lager:warning("rcvd unknown info msg: ~p", [_Msg]),
    {'noreply', State}.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

terminate(_Reason, _State) ->
    lager:error("~p terminated: ~p", [?MODULE, _Reason]),
    stop_listeners(),
    'ok'.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
start_listeners() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/device/:node_id", 'legacy_ws_handler', []}
            ,{"/device/:node_id/ws/push", 'legacy_ws_handler', []}
            ,{"/ota/:type/:node_id", 'legacy_ota_handler', []}
            ,{"/status", 'legacy_status_handler', []}
            ,{"/status/:type", 'legacy_status_handler', []}
            ,{"/status/nodes/:node", 'legacy_status_handler', []}
        ]}
    ]),
    CaCertFile = filename:join(code:priv_dir(?LEGACY), "ssl/old/sensity.crt"),
    CertFile = filename:join(code:priv_dir(?LEGACY), "ssl/old/server.pem"),
    KeyFile = filename:join(code:priv_dir(?LEGACY), "ssl/old/server-key.pem"),
    Ciphers = ["ECDHE-ECDSA-AES256-GCM-SHA384","ECDHE-RSA-AES256-GCM-SHA384",
                "ECDHE-ECDSA-AES256-SHA384","ECDHE-RSA-AES256-SHA384", "ECDHE-ECDSA-DES-CBC3-SHA",
                "ECDH-ECDSA-AES256-GCM-SHA384","ECDH-RSA-AES256-GCM-SHA384","ECDH-ECDSA-AES256-SHA384",
                "ECDH-RSA-AES256-SHA384","DHE-DSS-AES256-GCM-SHA384","DHE-DSS-AES256-SHA256",
                "AES256-GCM-SHA384","AES256-SHA256","ECDHE-ECDSA-AES128-GCM-SHA256",
                "ECDHE-RSA-AES128-GCM-SHA256","ECDHE-ECDSA-AES128-SHA256","ECDHE-RSA-AES128-SHA256",
                "ECDH-ECDSA-AES128-GCM-SHA256","ECDH-RSA-AES128-GCM-SHA256","ECDH-ECDSA-AES128-SHA256",
                "ECDH-RSA-AES128-SHA256","DHE-DSS-AES128-GCM-SHA256","DHE-DSS-AES128-SHA256",
                "AES128-GCM-SHA256","AES128-SHA256","ECDHE-ECDSA-AES256-SHA",
                "ECDHE-RSA-AES256-SHA","DHE-DSS-AES256-SHA","ECDH-ECDSA-AES256-SHA",
                "ECDH-RSA-AES256-SHA","AES256-SHA","ECDHE-ECDSA-AES128-SHA",
                "ECDHE-RSA-AES128-SHA","DHE-DSS-AES128-SHA","ECDH-ECDSA-AES128-SHA",
                "ECDH-RSA-AES128-SHA","AES128-SHA"],
    HttpsOpts = [
        {'port', 10443}
        ,{'cacertfile', CaCertFile}
        ,{'certfile', CertFile}
        ,{'keyfile', KeyFile}
        ,{'ciphers', Ciphers}
        ,{'keepalive', 'true'}
    ],
    {'ok', HttpsPid} = cowboy:start_tls(
        ?HTTPS_LISTENER
        ,100
        ,HttpsOpts
        ,#{'env' => #{'dispatch' => Dispatch}}
    ),
    lager:info("started https listeners ~p ~p", [HttpsOpts, HttpsPid]),

    HttpOpts = [
        {'port', 8080}
        ,{'keepalive', 'true'}
    ],
    {'ok', HttpPid} = cowboy:start_clear(
        ?HTTP_LISTENER
        ,100
        ,HttpOpts
        ,#{'env' => #{'dispatch' => Dispatch}}
    ),
    lager:info("started http listeners ~p ~p", [HttpOpts, HttpPid]),
    lager:info("LEGACY fully started").


stop_listeners() ->
    'ok' = cowboy:stop_listener(?HTTPS_LISTENER),
    'ok' = cowboy:stop_listener(?HTTP_LISTENER),
    'ok'.
