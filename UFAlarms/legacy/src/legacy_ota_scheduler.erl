%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Legacy OTA scheduler ==
%% Handle OTA jobs received from datadealer.
%%
%% It creates a worker queue to handle firmware update, it will `spwan_link'
%% a `legacy_ota_worker' for each node and listen for `Exist' messages and
%% publish OTA updates
%% @end
%%%-------------------------------------------------------------------
-module(legacy_ota_scheduler).

-behavior(gen_server).

-include("legacy.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    start_link/1
    ,gproc_key/1
    ,stop/1
    ,faster/1
    ,slower/1
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
    id
    ,q
    ,workers
    ,limit
    ,nodeids
    ,firmwareid
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?SERVER, Args, []).

%%--------------------------------------------------------------------
%% @doc
%% Return key used for gproc registration
%% @end
%%--------------------------------------------------------------------
-spec gproc_key(string()) -> {string(), ?MODULE}.
gproc_key(ID) ->
    {ID, ?MODULE}.

%%--------------------------------------------------------------------
%% @doc
%% Sends a `stop' message to `legacy_ota_scheduler'
%% @end
%%--------------------------------------------------------------------
-spec stop(map()) -> 'ok'.
stop(Map) ->
    ID = maps:get("jobid", Map),
    Key = legacy_ota_scheduler:gproc_key(ID),
    case gproc:lookup_global_name(Key) of
        'undefined' ->
            lager:warning("~p legacy_ota_scheduler not found", [ID]);
        Pid ->
            gen_server:cast(Pid, 'stop')
    end.

%%--------------------------------------------------------------------
%% @doc
%% Sends a `faster' message to `legacy_ota_scheduler'
%% @end
%%--------------------------------------------------------------------
-spec faster(map()) -> 'ok'.
faster(Map) ->
    ID = maps:get("jobid", Map),
    Key = legacy_ota_scheduler:gproc_key(ID),
    case gproc:lookup_global_name(Key) of
        'undefined' ->
            lager:warning("~p legacy_ota_scheduler not found", [ID]);
        Pid ->
            gen_server:cast(Pid, 'faster')
    end.

%%--------------------------------------------------------------------
%% @doc
%% Sends a `slower' message to `legacy_ota_scheduler'
%% @end
%%--------------------------------------------------------------------
-spec slower(map()) -> 'ok'.
slower(Map) ->
    ID = maps:get("jobid", Map),
    Key = legacy_ota_scheduler:gproc_key(ID),
    case gproc:lookup_global_name(Key) of
        'undefined' ->
            lager:warning("~p legacy_ota_scheduler not found", [ID]);
        Pid ->
            gen_server:cast(Pid, 'slower')
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([Config, Map, ID]=Args) ->
    process_flag('trap_exit', 'true'),
    legacy_helpers:init_lager(ID),
    lager:info("init ~p", [Args]),

    NodeIDs = maps:get("nodeid", Map, []),
    FirmwareID = maps:get("firmwareid", Map),
    Version = maps:get("model", Map),

    Bucket = proplists:get_value('bucket', Config, ?OTA_DEFAULT_S3_BUCKET),
    Host = proplists:get_value('host', Config, ?OTA_DEFAULT_HOST),
    Limit = proplists:get_value('workers', Config, ?OTA_DEFAULT_WORKERS),

    _ = legacy_ota_helpers:publish_success(ID, "unknown", FirmwareID, ?OTA_JOB_RECEIVED),

    self() ! {'check_firmware', Version, Bucket, Host},

    {'ok', #state{id=ID, nodeids=NodeIDs, firmwareid=FirmwareID, limit=Limit}}.

handle_call(_Msg, _From, State) ->
    lager:debug("rcvd unknown call msg: ~p from: ~p", [_Msg, _From]),
    {'reply', 'ok', State}.

handle_cast('faster', #state{limit=Limit}=State) ->
    NewLimit = Limit + ?OTA_DEFAULT_WORKERS,
    lager:info("rcvd faster message, limit is now ~p", [NewLimit]),
    self() ! 'maybe_start_worker',
    {'noreply', State#state{limit=NewLimit}};
handle_cast('slower', #state{limit=Limit}=State) ->
    case Limit - ?OTA_DEFAULT_WORKERS of
        NewLimit when NewLimit =< ?OTA_DEFAULT_WORKERS ->
            lager:warning("limit is already too low"),
            {'noreply', State};
        NewLimit ->
            lager:info("rcvd slower message, limit is now ~p", [NewLimit]),
            {'noreply', State#state{limit=NewLimit}}
    end;
handle_cast('stop', State) ->
    lager:info("rcvd message to stop"),
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    lager:debug("rcvd unknown cast msg: ~p", [_Msg]),
    {'noreply', State}.

handle_info({'check_firmware', Version, Bucket, Host}, #state{id=ID, firmwareid=FirmwareID, nodeids=NodeIDs, limit=Limit}=State) ->
    lager:debug("check_firmware ~p", [FirmwareID]),
    case legacy_ota_helpers:get_firmware(FirmwareID, Version, Bucket) of
        {'error', Reason} ->
            lager:warning("failed update firmware for [~p ~p] because ~p", [Version, FirmwareID, Reason]),
            {'stop', {'shutdown', ?OTA_FIRMWARE_NOT_FOUND}, State};
        {'ok', Files} ->
            lager:debug("found files ~p", [Files]),
            Q =
                lists:foldr(
                    fun(NodeID, Acc) ->
                        Args = legacy_ota_helpers:create_worker_args(
                            Version
                            ,Host
                            ,ID
                            ,NodeID
                            ,FirmwareID
                            ,Files
                        ),
                        queue:in(Args, Acc)
                    end
                    ,queue:new()
                    ,NodeIDs
                ),
            lager:debug("started with limit ~p Q ~p", [Limit, Q]),
            self() ! 'register',
            {'noreply', State#state{q=Q, workers=[]}}
    end;
handle_info('register', #state{id=ID}=State) ->
    lager:debug("register ~p", [ID]),
    case gproc_register(ID) of
        'error' -> {'stop', {'shutdown', ?OTA_FAIL_JOB_REGISTRATION}, State};
        'ok' ->
            self() ! 'maybe_start_worker',
            {'noreply', State}
    end;
handle_info('maybe_start_worker', #state{workers=Workers, limit=Limit}=State) ->
    case erlang:length(Workers) >= Limit of
        'true' ->
            lager:debug("workers limit reached ~p", [Limit]);
        'false' ->
            self() ! 'start_worker'
    end,
    {'noreply', State};
handle_info('start_worker', #state{q=Q, workers=Workers}=State) ->
    lager:debug("looking for work"),
    case queue:out(Q) of
        {{'value', Map}, Q2} ->
            {'ok', Pid} = legacy_ota_worker:start_link(Map),
            lager:debug("started work @ ~p", [Pid]),
            self() ! 'maybe_start_worker',
            {'noreply', State#state{q=Q2, workers=[{Pid, Map}|Workers]}};
        {'empty', _Q} ->
            lager:debug("no more work"),
            self() ! 'maybe_done',
            {'noreply', State}
    end;
handle_info('maybe_done', #state{workers=[]}=State) ->
    lager:info("job done"),
    {'stop', 'normal', State};
handle_info('maybe_done', State) ->
    lager:debug("still working"),
    {'noreply', State};
handle_info({'EXIT', Pid, 'normal'}, #state{workers=Workers}=State) ->
    lager:info("~p done working", [Pid]),
    self() ! 'maybe_start_worker',
    {'noreply', State#state{workers=proplists:delete(Pid, Workers)}};
handle_info({'EXIT', Pid, ?OTA_ALREADY_UPDATING}, #state{workers=Workers}=State) ->
    lager:info("~p reported update for node already running", [Pid]),
    self() ! 'maybe_start_worker',
    {'noreply', State#state{workers=proplists:delete(Pid, Workers)}};
handle_info({'EXIT', Pid, ?OTA_NODE_OFFLINE}, #state{workers=Workers}=State) ->
    lager:info("~p reported node is offline", [Pid]),
    self() ! 'maybe_start_worker',
    {'noreply', State#state{workers=proplists:delete(Pid, Workers)}};
handle_info({'EXIT', Pid, _Reason}, #state{q=Q, workers=Workers}=State) ->
    Map = proplists:get_value(Pid, Workers),
    NodeID = maps:get("nodeid", Map),
    lager:warning("~p reported an error ~p", [NodeID, _Reason]),
    {Q1, W1} =
        case maps:get("retry", Map) of
            0 ->
                lager:error("too many retries for ~p, dropping", [NodeID]),
                {Q, proplists:delete(Pid, Workers)};
            Retry ->
                Map1 = maps:update("retry", Retry-1, Map),
                lager:info("retrying ~p [~p]", [NodeID, Retry]),
                {queue:in_r(Map1, Q), proplists:delete(Pid, Workers)}
        end,
    self() ! 'maybe_start_worker',
    {'noreply', State#state{q=Q1, workers=W1}};
handle_info(_Msg, State) ->
    lager:debug("rcvd unknown info msg: ~p", [_Msg]),
    {'noreply', State}.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

terminate({'shutdown', ?OTA_FIRMWARE_NOT_FOUND}, #state{id=ID, firmwareid=FirmwareID}) ->
    lager:error("~p terminated: ~p", [?MODULE, ?OTA_FIRMWARE_NOT_FOUND]),
    legacy_ota_helpers:publish_failure(ID, "unknown", FirmwareID, ?OTA_FIRMWARE_NOT_FOUND),
    legacy_ota_sup:terminate_child(ID),
    'ok';
terminate({'shutdown', ?OTA_FAIL_JOB_REGISTRATION}, #state{id=ID, firmwareid=FirmwareID}) ->
    lager:error("~p terminated: ~p", [?MODULE, ?OTA_FAIL_JOB_REGISTRATION]),
    legacy_ota_helpers:publish_failure(ID, "unknown", FirmwareID, ?OTA_FAIL_JOB_REGISTRATION),
    legacy_ota_sup:terminate_child(ID),
    'ok';
terminate('normal', #state{id=ID, firmwareid=FirmwareID}) ->
    lager:info("~p terminated", [?MODULE]),
    legacy_ota_helpers:publish_success(ID, "unknown", FirmwareID, ?OTA_JOB_DONE),
    legacy_ota_sup:terminate_child(ID),
    'ok';
terminate(_Reason, #state{id=ID}) ->
    lager:info("~p terminated: ~p", [?MODULE, _Reason]),
    legacy_ota_sup:terminate_child(ID),
    'ok'.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec gproc_register(string()) -> 'ok' | 'error'.
gproc_register(ID) ->
    Self = self(),
    Key = gproc_key(ID),
    lager:debug("registering ~p to ~p", [Key, Self]),
    case gproc:lookup_global_name(Key) of
        'undefined' ->
            try
                gproc:add_global_name(Key),
                'ok'
            catch
                'error':_E ->
                    lager:error("failed to register node ~p : ~p", [ID, _E]),
                    'error'
            end;
        Self ->
            lager:debug("~p is already registered with ~p", [Key, Self]);
        Other ->
            lager:debug("~p is register with another process ~p", [Key, Other]),
            'error'
    end.
