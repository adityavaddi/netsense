-module(ota_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("legacy.hrl").
-include("messages_pb.hrl").

-export([
    all/0
    ,groups/0
    ,init_per_suite/1 ,end_per_suite/1
    ,init_per_testcase/2 ,end_per_testcase/2
]).

-export([
    node_offline/1
    ,firmware_not_found/1
    ,happy_case_v4/1
    ,happy_case_v6/1
    ,retry/1
]).

-define(NODEID, "NS_1").
-define(MCU_ID, "fw001").
-define(MODEM_ID, "m003").
-define(NODEID_BIN, <<"NS_1">>).
-define(MCU_ID_BIN, <<"fw001">>).
-define(MODEM_ID_BIN, <<"m003">>).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Running tests for this suite
%% @end
%%--------------------------------------------------------------------
all() ->
    [{'group', 'happy_cases'}, {'group', 'failed_cases'}].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Tests groups
%% @end
%%--------------------------------------------------------------------
groups() ->
        [{'failed_cases', [], ['node_offline', 'firmware_not_found', 'retry']}
     ,{'happy_cases', [], ['happy_case_v4', 'happy_case_v6']}].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special init config for suite
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ct:pal("START OF ~p", [?MODULE]),

    test_helpers:init_legacy(),
    test_helpers:mock_deps(),

    'ok' = meck:new('legacy_helpers', ['passthrough', 'no_link']),
    'ok' = meck:expect('legacy_helpers', 'time', fun() ->
        ct:pal("legacy_helpers:timer()", []),
        0
    end),

    {'ok', _} = application:ensure_all_started('legacy'),

    os:cmd("rm -Rf " ++ "/tmp/legacy/" ++ ?MCU_ID),


    ModemFile = string:join(["/tmp/legacy/", ?MCU_ID, "/modem-1.0-unode-v6-", ?MODEM_ID, ".zip"], ""),
    BinFileV4 = string:join(["/tmp/legacy/", ?MCU_ID, "/micronode-2.2-unode-v4-", ?MCU_ID, ".bin"], ""),
    FtfsFileV4 = string:join(["/tmp/legacy/", ?MCU_ID, "/micronode-2.2-unode-v4-", ?MCU_ID, ".ftfs"], ""),
    BinFileV6 = string:join(["/tmp/legacy/", ?MCU_ID, "/micronode-2.2-unode-v6-", ?MCU_ID, ".bin"], ""),
    FtfsFileV6 = string:join(["/tmp/legacy/", ?MCU_ID, "/micronode-2.2-unode-v6-", ?MCU_ID, ".ftfs"], ""),

    'ok' = file:make_dir("/tmp/legacy/" ++ ?MCU_ID),
    'ok' = file:write_file(ModemFile, <<>>),
    'ok' = file:write_file(BinFileV4, <<>>),
    'ok' = file:write_file(FtfsFileV4, <<>>),
    'ok' = file:write_file(BinFileV6, <<>>),
    'ok' = file:write_file(FtfsFileV6, <<>>),

    Config.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special end config for groups
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    test_helpers:stop_legacy(),
    test_helpers:unmock(),
    ct:pal("END OF ~p", [?MODULE]),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special init config for suite
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_Case, Config) ->
    ct:pal("START OF ~p", [_Case]),
    test_helpers:mock_rabbit_helper(),
    Config.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special end config for groups
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_Case, _Config) ->
    meck:unload('rabbit_helper'),
    ct:pal("END OF ~p", [_Case]),
    'ok'.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Check command is rcv by server
%% @end
%%--------------------------------------------------------------------
node_offline(_Config) ->
    ID = "111-222-333-444",
    legacy_cmd_handler:handle(
        msgpack:pack(#{
            "name" => "AssignFirmware"
            ,"nodeid" => [?NODEID]
            ,"firmwareid" => ?MCU_ID
            ,"model" => "v4"
            ,"jobid" => ID
        })
    ),
    test_helpers:expect_published([
        {<<"node.events">>, <<"111-222-333-444.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_JOB_RECEIVED
        }}
        ,{<<"node.events">>, <<"111-222-333-444.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"nodeid" => ?NODEID
            ,"jobid" => ID
            ,"success" => 'false'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_NODE_OFFLINE
        }}
        ,{<<"node.events">>, <<"111-222-333-444.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_JOB_DONE
        }}
    ]),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Check command is rcv by server
%% @end
%%--------------------------------------------------------------------
firmware_not_found(_Config) ->
    ID = "555-666-777-888",
    legacy_cmd_handler:handle(
        msgpack:pack(#{
            "name" => "AssignFirmware"
            ,"nodeid" => [?NODEID]
            ,"firmwareid" => ?MCU_ID
            ,"model" => "v5"
            ,"jobid" => ID
        })
    ),
    test_helpers:expect_published([
        {<<"node.events">>, <<"555-666-777-888.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_JOB_RECEIVED
        }}
        ,{<<"node.events">>, <<"555-666-777-888.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"success" => 'false'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_FIRMWARE_NOT_FOUND
        }}
    ]),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Check happy case for a v4 node
%% @end
%%--------------------------------------------------------------------
happy_case_v4(_Config) ->
    ConnPid = connect_node(),

    ID = "0123-4567-8910",
    legacy_cmd_handler:handle(
        msgpack:pack(#{
            "name" => "AssignFirmware"
            ,"nodeid" => [?NODEID]
            ,"firmwareid" => ?MCU_ID
            ,"model" => "v4"
            ,"jobid" => ID
        })
    ),
    _ = command_loop(<<"https://127.0.0.1:10443/ota/mcu/", ?NODEID_BIN/binary, ".bin">>
                    ,'undefined'),
    timer:sleep(100),
    gun:close(ConnPid),
    _ = download_file("mcu", ".bin"),
    LoginReq = loginReq:test_record(?NODEID, [{"clientType", <<"unode-v4">>}
                                              ,{"swVerId", ?MCU_ID_BIN}]),
    ConnPid2 = login_req(LoginReq),

    test_helpers:expect_published([
        {<<"node.events">>, <<?NODEID_BIN/binary, ".login.connected">>, connection_status:test_map(?NODEID, "connected")}
        ,{<<"node.events">>, <<"0123-4567-8910.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_JOB_RECEIVED
        }}
        ,{<<"node.events">>, <<"0123-4567-8910.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_COMMAND_SENT
        }}
        ,{<<"node.events">>, <<?NODEID_BIN/binary, ".login.disconnected">>, #{
            "name" => "ConnectionStatus"
            ,"nodeid" => ?NODEID
            ,"status" => "disconnected"
        }}
        ,{<<"node.events">>, <<"0123-4567-8910.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_NODE_REBOOTING
        }}
        ,{<<"node.events">>, <<"0123-4567-8910.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"file" => "/tmp/legacy/" ++ ?MCU_ID ++ "/micronode-2.2-unode-v4-fw001.bin"
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_START_DOWNLOAD
        }}
        ,{<<"node.events">>, <<"0123-4567-8910.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"file" => "/tmp/legacy/" ++ ?MCU_ID ++ "/micronode-2.2-unode-v4-fw001.bin"
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_STOP_DOWNLOAD
        }}
        ,{<<"node.events">>, <<?NODEID_BIN/binary, ".login.req">>, loginReq:test_map(?NODEID, [{"clientType", "unode-v4"}
                                                                                 ,{"swVerId", ?MCU_ID}])}
        ,{<<"node.events">>, <<?NODEID_BIN/binary, ".login.connected">>, connection_status:test_map(?NODEID, "connected")}
        ,{<<"node.events">>, <<"0123-4567-8910.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_UPDATE_SUCCESSFUL
        }}
        ,{<<"node.events">>, <<"0123-4567-8910.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_JOB_DONE
        }}
    ]),
    gun:close(ConnPid2),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Check happy case for a v4 node
%% @end
%%--------------------------------------------------------------------
happy_case_v6(_Config) ->
    ConnPid = connect_node(),
    ID = "1111-2222-3333",
    legacy_cmd_handler:handle(
        msgpack:pack(#{
            "name" => "AssignFirmware"
            ,"nodeid" => [?NODEID]
            ,"firmwareid" => ?MCU_ID
            ,"model" => "v6"
            ,"jobid" => ID
        })
    ),
    _ = command_loop(<<"https://127.0.0.1:10443/ota/mcu/", ?NODEID_BIN/binary, ".bin">>
                    ,<<"http://127.0.0.1:8080/ota/modem/", ?NODEID_BIN/binary, ".zip">>),

    timer:sleep(100),
    gun:close(ConnPid),
    _ = download_file("mcu", ".bin"),
    _ = download_file("modem", ".zip"),
    LoginReq = loginReq:test_record(?NODEID, [{"clientType", <<"unode-v6">>}
                                              ,{"swVerId", ?MCU_ID_BIN}
                                              ,{"modemRevEd", ?MODEM_ID_BIN}]),
    ConnPid2 = login_req(LoginReq),

    test_helpers:expect_published([
        {<<"node.events">>, <<?NODEID_BIN/binary, ".login.connected">>, connection_status:test_map(?NODEID, "connected")}
        ,{<<"node.events">>, <<"1111-2222-3333.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_JOB_RECEIVED
        }}
        ,{<<"node.events">>, <<"1111-2222-3333.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_COMMAND_SENT
        }}
        ,{<<"node.events">>, <<"1111-2222-3333.ota.status">>, #{
            "firmwareid" => ?MODEM_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_COMMAND_SENT
        }}
        ,{<<"node.events">>, <<"1111-2222-3333.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_NODE_REBOOTING
        }}
        ,{<<"node.events">>, <<?NODEID_BIN/binary, ".login.disconnected">>, #{
            "name" => "ConnectionStatus"
            ,"nodeid" => ?NODEID
            ,"status" => "disconnected"
        }}
        ,{<<"node.events">>, <<"1111-2222-3333.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"file" => "/tmp/legacy/" ++ ?MCU_ID ++ "/micronode-2.2-unode-v6-fw001.bin"
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_START_DOWNLOAD
        }}
        ,{<<"node.events">>, <<"1111-2222-3333.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"file" => "/tmp/legacy/" ++ ?MCU_ID ++ "/micronode-2.2-unode-v6-fw001.bin"
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_STOP_DOWNLOAD
        }}
        ,{<<"node.events">>, <<"1111-2222-3333.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"file" => "/tmp/legacy/" ++ ?MCU_ID ++ "/modem-1.0-unode-v6-m003.zip"
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_START_DOWNLOAD
        }}
        ,{<<"node.events">>, <<"1111-2222-3333.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"file" => "/tmp/legacy/" ++ ?MCU_ID ++ "/modem-1.0-unode-v6-m003.zip"
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_STOP_DOWNLOAD
        }}
        ,{<<"node.events">>, <<?NODEID_BIN/binary, ".login.req">>, loginReq:test_map(?NODEID, [{"clientType", "unode-v6"}
                                                                               ,{"swVerId", ?MCU_ID}
                                                                               ,{"modemRevEd", ?MODEM_ID}])}
        ,{<<"node.events">>, <<?NODEID_BIN/binary, ".login.connected">>, connection_status:test_map(?NODEID, "connected")}
        ,{<<"node.events">>, <<"1111-2222-3333.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_UPDATE_SUCCESSFUL
        }}
        ,{<<"node.events">>, <<"1111-2222-3333.ota.status">>, #{
            "firmwareid" => ?MODEM_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_UPDATE_SUCCESSFUL
        }}
        ,{<<"node.events">>, <<"1111-2222-3333.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_JOB_DONE
        }}
    ]),

    gun:close(ConnPid2),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Check retry if update if failing
%%   Should check 3 times
%% @end
%%--------------------------------------------------------------------
retry(_Config) ->
    ConnPid = connect_node(),
    ID = "some-random-id",
    legacy_cmd_handler:handle(
        msgpack:pack(#{
            "name" => "AssignFirmware"
            ,"nodeid" => [?NODEID]
            ,"firmwareid" => ?MCU_ID
            ,"model" => "v4"
            ,"jobid" => ID
        })
    ),
    ConnPid2 = do_retry(3, ConnPid),
    Results = [
        {<<"node.events">>, <<?NODEID_BIN/binary, ".login.connected">>, connection_status:test_map(?NODEID, "connected")}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_JOB_RECEIVED
        }}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_COMMAND_SENT
        }}
        ,{<<"node.events">>, <<?NODEID_BIN/binary, ".login.disconnected">>, #{
            "name" => "ConnectionStatus"
            ,"nodeid" => ?NODEID
            ,"status" => "disconnected"
        }}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_NODE_REBOOTING
        }}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"file" => "/tmp/legacy/" ++ ?MCU_ID ++ "/micronode-2.2-unode-v4-fw001.bin"
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_START_DOWNLOAD
        }}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"file" => "/tmp/legacy/" ++ ?MCU_ID ++ "/micronode-2.2-unode-v4-fw001.bin"
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_STOP_DOWNLOAD
        }}
        ,{<<"node.events">>, <<?NODEID_BIN/binary, ".login.req">>, loginReq:test_map(?NODEID, [{"clientType", "unode-v4"}
                                                                               ,{"swVerId", "wrongID"}])}
        ,{<<"node.events">>, <<?NODEID_BIN/binary, ".login.connected">>, connection_status:test_map(?NODEID, "connected")}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'false'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_UPDATE_FAILED
        }}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_COMMAND_SENT
        }}
        ,{<<"node.events">>, <<?NODEID_BIN/binary, ".login.disconnected">>, #{
            "name" => "ConnectionStatus"
            ,"nodeid" => ?NODEID
            ,"status" => "disconnected"
        }}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_NODE_REBOOTING
        }}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"file" => "/tmp/legacy/" ++ ?MCU_ID ++ "/micronode-2.2-unode-v4-fw001.bin"
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_START_DOWNLOAD
        }}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"file" => "/tmp/legacy/" ++ ?MCU_ID ++ "/micronode-2.2-unode-v4-fw001.bin"
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_STOP_DOWNLOAD
        }}
        ,{<<"node.events">>, <<?NODEID_BIN/binary, ".login.req">>, loginReq:test_map(?NODEID, [{"clientType", "unode-v4"}
                                                                               ,{"swVerId", "wrongID"}])}
        ,{<<"node.events">>, <<?NODEID_BIN/binary, ".login.connected">>, connection_status:test_map(?NODEID, "connected")}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'false'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_UPDATE_FAILED
        }}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_COMMAND_SENT
        }}
        ,{<<"node.events">>, <<?NODEID_BIN/binary, ".login.disconnected">>, #{
            "name" => "ConnectionStatus"
            ,"nodeid" => ?NODEID
            ,"status" => "disconnected"
        }}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_NODE_REBOOTING
        }}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"file" => "/tmp/legacy/" ++ ?MCU_ID ++ "/micronode-2.2-unode-v4-fw001.bin"
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_START_DOWNLOAD
        }}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"file" => "/tmp/legacy/" ++ ?MCU_ID ++ "/micronode-2.2-unode-v4-fw001.bin"
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_STOP_DOWNLOAD
        }}
        ,{<<"node.events">>, <<?NODEID_BIN/binary, ".login.req">>, loginReq:test_map(?NODEID, [{"clientType", "unode-v4"}
                                                                               ,{"swVerId", "wrongID"}])}
        ,{<<"node.events">>, <<?NODEID_BIN/binary, ".login.connected">>, connection_status:test_map(?NODEID, "connected")}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"nodeid" => ?NODEID
            ,"success" => 'false'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_UPDATE_FAILED
        }}
        ,{<<"node.events">>, <<"some-random-id.ota.status">>, #{
            "firmwareid" => ?MCU_ID
            ,"jobid" => ID
            ,"success" => 'true'
            ,"name" => "OTAStatus"
            ,"when" => 0
            ,"status" => ?OTA_JOB_DONE
        }}
    ],
    test_helpers:expect_published(Results),
    gun:close(ConnPid2),
    'ok'.

do_retry(0, ConnPid) -> ConnPid;
do_retry(Try, ConnPid) ->
    ct:pal("retry ~p", [Try]),
    _ = command_loop(<<"https://127.0.0.1:10443/ota/mcu/", ?NODEID_BIN/binary, ".bin">>
                     ,'undefined'),
    gun:close(ConnPid),
    _ = download_file("mcu", ".bin"),
    LoginReq = loginReq:test_record(?NODEID, [{"swVerId", <<"wrongID">>}]),
    ConnPid2 = login_req(LoginReq),
    timer:sleep(100),
    do_retry(Try-1, ConnPid2).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
connect_node() ->
    {'ok', ConnPid} = gun:open("localhost", 8080),
    gun:ws_upgrade(ConnPid, "/device/" ++ ?NODEID),
    'ok' = connect_node_loop(),
    ConnPid.

connect_node_loop() ->
    receive
        {'gun_up', _ConnPid, 'http'} ->
            connect_node_loop();
        {'gun_ws_upgrade', _ConnPid, 'ok', _Header} ->
            'ok';
        {'gun_error', _ConnPid, _StreamRef, Reason} ->
            ct:fail(Reason);
        _Msg ->
            ct:pal("unhandled message ~p", [_Msg]),
            ct:fail("unhandled message")
    after 1000 ->
        ct:fail("timeout")
    end.

command_loop(Url, Url2) ->
    receive
        {'gun_ws', _ConnPid, {'binary', Bin}} ->
            Envelope = envelope:decode(Bin),
            Req = softwareUpdateReq:create(Url, Url2),
            {'softwareUpdateReq', Req} = envelope:type(Envelope),
            ct:pal("node received software update req with ~p with ~p", [Url, Req]);
        _Msg ->
            ct:pal("unhandled message ~p", [_Msg]),
            ct:fail("command_loop unhandled message")
    after 1000 ->
        ct:fail("command_loop timeout")
    end.

download_file(Type, Ext) ->
    {'ok', ConnPid2} = gun:open("localhost", 8080),
    _StreamRef = gun:get(ConnPid2, "/ota/" ++ Type ++ "/" ++ ?NODEID ++ Ext),
    _ = download_file_loop(),
    timer:sleep(250),
    gun:close(ConnPid2).

download_file_loop() ->
    receive
        {'gun_up', _ConnPid, 'http'} ->
            download_file_loop();
        {'gun_data', _ConnPid, _StreamRef, 'nofin', Data} ->
            ct:pal("~p", [Data]),
            download_file_loop();
        {'gun_data', _ConnPid, _StreamRef, 'fin', Data} ->
            ct:pal("~p", [Data]);
        {'gun_response', _ConnPid, _StreamRef, 'fin', _Code, Data} ->
            ct:pal("~p", [Data]);
        _Msg ->
            ct:pal("unhandled message ~p", [_Msg]),
            ct:fail("unhandled message")
    after 1000 ->
        ct:fail("timeout")
    end.

login_req(LoginReq) ->
    {'ok', ConnPid} = gun:open("localhost", 8080),
    gun:ws_upgrade(ConnPid, "/device/" ++ ?NODEID),
    login_req_loop(LoginReq),
    ConnPid.

login_req_loop(LoginReq) ->
    receive
        {'gun_up', _ConnPid, 'http'} ->
            login_req_loop(LoginReq);
        {'gun_ws_upgrade', ConnPid, 'ok', _Header} ->
            Envelope = envelope:create(LoginReq),
            Encoded = envelope:encode(Envelope),
            'ok' = gun:ws_send(ConnPid, {'binary', Encoded}),
            login_req_loop(LoginReq);
        {'gun_error', _ConnPid, _StreamRef, Reason} ->
            ct:fail(Reason);
        {'gun_ws', _ConnPid, {'binary', Bin}} ->
            Envelope = envelope:decode(Bin),
            {'loginResp', _} = envelope:type(Envelope),
            'ok';
        _Msg ->
            ct:pal("unhandled message ~p", [_Msg]),
            ct:fail("unhandled message")
    after 1000 ->
        ct:fail("timeout")
    end.
