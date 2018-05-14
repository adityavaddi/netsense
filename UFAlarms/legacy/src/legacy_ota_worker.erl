%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Legacy OTA Worker ==
%% Handle firmware update per node.
%%
%% Send command to node, hold status and publish OTA updates
%% @end
%%%-------------------------------------------------------------------
-module(legacy_ota_worker).

-behavior(gen_server).

-include("legacy.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    start_link/1
    ,get_info/1
    ,handle_node_reboot/1
    ,handle_file_download/4
    ,handle_login_req/2
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
    jobid
    ,version
    ,nodeid
    ,host
    ,firmwares
    ,wspid
    ,status
}).

-type 'state'() :: #'state'{}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?SERVER, Args, []).

%%--------------------------------------------------------------------
%% @doc
%% Return worker's info
%% @end
%%--------------------------------------------------------------------
-spec get_info(string()) -> {'error', 'not_found'} | {'ok', any()}.
get_info(NodeID) ->
    Key = gproc_key(NodeID),
    case gproc:lookup_global_name(Key) of
        'undefined' ->
            lager:debug("no ota worker registered for ~p", [NodeID]),
            {'error', 'not_found'};
        Pid ->
            gen_server:call(Pid, 'get_info')
    end.

%%--------------------------------------------------------------------
%% @doc
%% Send status update to `legacy_ota_worker'
%% @end
%%--------------------------------------------------------------------
-spec handle_node_reboot(string()) -> 'ok'.
handle_node_reboot(NodeID) ->
    Key = gproc_key(NodeID),
    case gproc:lookup_global_name(Key) of
        'undefined' ->
            lager:debug("no ota worker registered for ~p", [NodeID]);
        Pid ->
            Pid ! 'node_rebooting'
    end.

%%--------------------------------------------------------------------
%% @doc
%% Send status update to `legacy_ota_worker'
%% @end
%%--------------------------------------------------------------------
-spec handle_file_download(string(), string(), string(), string()) -> 'ok'.
handle_file_download(NodeID, Step, ID, File) ->
    Key = gproc_key(NodeID),
    case gproc:lookup_global_name(Key) of
        'undefined' ->
            lager:debug("no ota worker registered for ~p", [NodeID]);
        Pid ->
            Pid ! {'file_downloading', Step, ID, File}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Handle login req
%% @end
%%--------------------------------------------------------------------
-spec handle_login_req(string(), loginReq:'LoginReq'()) -> 'ok'.
handle_login_req(NodeID, LoginReq) ->
    Key = gproc_key(NodeID),
    case gproc:lookup_global_name(Key) of
        'undefined' ->
            lager:debug("no ota worker registered for ~p", [NodeID]);
        Pid ->
            Pid ! {'login_req', LoginReq}
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(Args) ->
    JobID = maps:get("jobid", Args),
    Version = maps:get("version", Args),
    NodeID = maps:get("nodeid", Args),
    Host = maps:get("host", Args),
    Firmwares = maps:get("firmwares", Args),

    legacy_helpers:init_lager(NodeID),
    lager:debug("init with ~p", [Args]),

    State = #state{
        jobid=JobID
        ,version=Version
        ,nodeid=NodeID
        ,host=Host
        ,firmwares=Firmwares
        ,status="init"
    },
    self() ! 'register',
    {'ok', State}.


handle_call('get_info', _From, State) ->
    lager:debug("rcvd get_info msg from: ~p", [ _From]),
    {'reply', {'ok', state_to_map(State)}, State};
handle_call(_Msg, _From, State) ->
    lager:debug("rcvd unknown call msg: ~p from: ~p", [_Msg, _From]),
    {'reply', 'ok', State}.

handle_cast(_Msg, State) ->
    lager:debug("rcvd unknown cast msg: ~p", [_Msg]),
    {'noreply', State}.

handle_info('register', #state{nodeid=NodeID}=State) ->
    lager:debug("register ~p", [NodeID]),
    case gproc_register(NodeID) of
        {'error', Reason} ->
            {'stop', Reason, State#state{status=Reason}};
        'ok' ->
            self() ! 'check_online',
            {'noreply', State}
    end;
handle_info('check_online', #state{nodeid=NodeID}=State) ->
    lager:debug("check if node ~p is online", [NodeID]),
    case legacy_helpers:node_online(NodeID) of
        'false' ->
            {'stop', ?OTA_NODE_OFFLINE, State#state{status=?OTA_NODE_OFFLINE}};
        {'true', WSPid} ->
            self() ! 'send_command',
            {'noreply', State#state{wspid=WSPid}}
    end;
handle_info('send_command', #state{jobid=JobID, nodeid=NodeID, host=Host
                                   ,firmwares=Firmwares, wspid=WSPid}=State) ->
    Cmd = legacy_ota_helpers:create_ota_command(Host, NodeID, Firmwares),
    WSPid ! {'command', Cmd},
    lists:foreach(
        fun({_Type, ID}) ->
            'ok' = legacy_ota_helpers:publish_success(JobID, NodeID, ID, ?OTA_COMMAND_SENT)
        end
        ,Firmwares
    ),
    {'noreply', State#state{status=?OTA_COMMAND_SENT}};
handle_info('node_rebooting', #state{jobid=JobID, nodeid=NodeID, firmwares=Fws}=State) ->
    lager:debug("node is now rebooting"),
    FwID = legacy_ota_helpers:get_mcu_id(Fws),
    'ok' = legacy_ota_helpers:publish_success(JobID, NodeID, FwID, ?OTA_NODE_REBOOTING),
    {'noreply', State#state{status=?OTA_NODE_REBOOTING}};
handle_info({'file_downloading', Step, ID, File}, #state{jobid=JobID, nodeid=NodeID}=State) ->
    Status = #{
        "status" => Step
        ,"file" => File
    },
    'ok' = legacy_ota_helpers:publish_success(JobID, NodeID, ID, Status),
    {'noreply', State#state{status=Status}};
handle_info({'login_req', LoginReq}, #state{jobid=JobID, nodeid=NodeID, firmwares=Firmwares}=State) ->
    Results =
        lists:foldl(
            fun({Type, ID}, Acc) ->
                case loginReq:get_fw_id(Type, LoginReq) of
                    ID ->
                        'ok' = legacy_ota_helpers:publish_success(JobID, NodeID, ID, ?OTA_UPDATE_SUCCESSFUL),
                        ['ok' | Acc];
                    _Else ->
                        'ok' = legacy_ota_helpers:publish_failure(JobID, NodeID, ID, ?OTA_UPDATE_FAILED),
                        ['error' | Acc]
                end
            end
            ,[]
            ,Firmwares
        ),
    case lists:member('error', Results) of
        'true' -> {'stop', ?OTA_UPDATE_FAILED, State#state{status=?OTA_UPDATE_FAILED}};
        'false' -> {'stop', 'normal', State}
    end;
handle_info(_Msg, State) ->
    lager:debug("rcvd unknown info msg: ~p", [_Msg]),
    {'noreply', State}.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

terminate('normal', _State) ->
    lager:debug("terminated normally");
terminate(_Reason, #state{status=?OTA_UPDATE_FAILED}) ->
    lager:warning("terminated: ~p", [_Reason]);
terminate(_Reason, #state{jobid=JobID, nodeid=NodeID, firmwares=Fws, status=Status}) ->
    lager:warning("terminated: ~p", [_Reason]),
    FwID = legacy_ota_helpers:get_mcu_id(Fws),
    'ok' = legacy_ota_helpers:publish_failure(JobID, NodeID, FwID, Status).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec gproc_register(string()) -> 'ok' | 'error'.
gproc_register(NodeID) ->
    Self = self(),
    Key = gproc_key(NodeID),
    lager:debug("registering ~p to ~p", [Key, Self]),
    case gproc:lookup_global_name(Key) of
        'undefined' ->
            try
                gproc:add_global_name(Key),
                'ok'
            catch
                'error':_E ->
                    lager:error("failed to register node ~p : ~p", [NodeID, _E]),
                    _ = try_unregister(Key),
                    {'error', ?OTA_FAIL_REGISTRATION}
            end;
        Self ->
            lager:debug("~p is already registered with ~p", [Key, Self]);
        Other ->
            lager:warning("~p is register with another process ~p", [Key, Other]),
            {'error', ?OTA_ALREADY_UPDATING}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec try_unregister({binary(), 'legacy_ota_worker'}) -> 'ok'.
try_unregister(Key) ->
    try
        gproc:unreg({'n', 'g', Key}),
        'ok'
    catch
        'error':_E ->
            lager:error("failed to unregister key ~p : ~p", [Key, _E]),
            'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec gproc_key(string()) -> {binary(), ?MODULE}.
gproc_key(NodeID) ->
    {legacy_helpers:symbol_to_binary(NodeID), ?MODULE}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec state_to_map(state()) -> map().
state_to_map(State) ->
    #{
        "jobid" => State#state.jobid
        ,"version" => State#state.version
        ,"nodeid" => State#state.nodeid
        ,"host" => State#state.host
        ,"firmwares" => State#state.firmwares
        ,"wspid" => State#state.wspid
        ,"status" => State#state.status
    }.
