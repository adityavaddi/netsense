%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Legacy OTA helpers ==
%% Series of helper functions for OTA
%% @end
%%%-------------------------------------------------------------------
-module(legacy_ota_helpers).

-export([
    get_firmware/2, get_firmware/3
    ,download_url/4
    ,publish_success/4 ,publish_failure/4
    ,create_worker_args/6
    ,create_ota_command/3
    ,get_mcu_id/1
]).

-include("legacy.hrl").

-define(RETRY_POLICY, 2).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%--------------------------------------------------------------------
%% @doc
%% Fetch firmware files (locally or from S3)
%% @end
%%--------------------------------------------------------------------
-spec get_firmware(string(), string()) -> {'ok', [string(), ...]} | {'error', atom()}.
get_firmware(FirmwareID, Version) ->
    {'ok', OTAConfig} = legacy_config:get('ota'),
    Bucket = proplists:get_value('bucket', OTAConfig, ?OTA_DEFAULT_S3_BUCKET),
    get_firmware(FirmwareID, Version, Bucket).

%%--------------------------------------------------------------------
%% @doc
%% Fetch firmware files (locally or from S3)
%% @end
%%--------------------------------------------------------------------
-spec get_firmware(string(), string(), string()) -> {'ok', [string(), ...]} | {'error', atom()}.
get_firmware(FirmwareID, Version, Bucket) ->
    {'ok', Files} = file:list_dir(?STORAGE_DIR),
    case legacy_helpers:filter_by(Files, FirmwareID) of
        [] ->
            lager:info("did not find any file matching ~p, trying download from s3 bucket ~p", [FirmwareID, Bucket]),
            case legacy_s3_worker:get_file_from_bucket(Bucket, FirmwareID, [{'unzip', 'true'}]) of
                {'ok', Dir} ->
                    get_firmware_from_directory(FirmwareID, Version, Dir);
                _Error -> _Error
            end;
        [Dir] ->
            get_firmware_from_directory(FirmwareID, Version, Dir);
        _Files ->
            lager:error("too many files found for ~p - ~p. Result: ~p", [Version, FirmwareID, _Files]),
            {'error', 'too_many_files'}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Return firmware download url for node
%% @end
%%--------------------------------------------------------------------
-spec download_url(string(), string(), string(), string()) -> binary().
download_url(Host, "modem"=Type, NodeID, Ext) ->
    erlang:iolist_to_binary([
        "http://", Host, ":8080/ota/"
        ,Type, "/", NodeID, ".", Ext
    ]);
download_url(Host, Type, NodeID, Ext) ->
    erlang:iolist_to_binary([
        "https://", Host, ":10443/ota/"
        ,Type, "/", NodeID, ".", Ext
    ]).

%%--------------------------------------------------------------------
%% @doc
%% Publish successful OTA update status
%% @end
%%--------------------------------------------------------------------
-spec publish_success(string(), string(), string(), map() | string()) -> 'ok'.
publish_success(JobID, NodeID, FirmwareID, Update) ->
    publish_status('true', JobID, NodeID, FirmwareID, Update).

%%--------------------------------------------------------------------
%% @doc
%% Publish failure OTA update status
%% @end
%%--------------------------------------------------------------------
-spec publish_failure(string(), string(), string(), map() | string()) -> 'ok'.
publish_failure(JobID, NodeID, FirmwareID, Update) ->
    publish_status('false', JobID, NodeID, FirmwareID, Update).

%%--------------------------------------------------------------------
%% @doc
%% Create map argugments depending on node version
%% @end
%%--------------------------------------------------------------------
-spec create_worker_args(string(), string(), string()
                         ,string(), string(), [string(), ...]) -> map().
create_worker_args("v6"=Version, Host, JobID, NodeID, FirmwareID, Files) ->
    case legacy_helpers:filter_by(Files, "modem") of
        [] ->
            lager:debug("no modem file found for ~p", [FirmwareID]),
            default_args(Host, JobID, Version, NodeID, FirmwareID);
        [Path] ->
            ModemID = get_id_from_file(Path),
            modem_args(Host, JobID, Version, NodeID, FirmwareID, ModemID);
        _Paths ->
            lager:warning("too many modem files found for ~p", [FirmwareID]),
            default_args(Host, JobID, Version, NodeID, FirmwareID)
    end;
create_worker_args(Version, Host, JobID, NodeID, FirmwareID, _Files) ->
    default_args(Host, JobID, Version, NodeID, FirmwareID).

%%--------------------------------------------------------------------
%% @doc
%% Create ota command base of type
%% @end
%%--------------------------------------------------------------------
-spec create_ota_command(string(), string(), any()) -> binary().
create_ota_command(Host, NodeID, Firmwares) ->
    MCU = proplists:get_value("mcu", Firmwares, 'undefined'),
    Modem = proplists:get_value("modem", Firmwares, 'undefined'),
    Url1 = create_ota_command(Host, NodeID, "mcu", MCU),
    Url2 = create_ota_command(Host, NodeID, "modem", Modem),

    Req = softwareUpdateReq:create(Url1, Url2),
    lager:debug("softwareUpdateReq ~p", [Req]),
    Envelope = envelope:create(Req),
    envelope:encode(Envelope).

-spec create_ota_command(string(), string(), string(), any()) -> binary() | 'undefined'.
create_ota_command(_Host, _NodeID, "mcu", 'undefined') ->
    'undefined';
create_ota_command(_Host, _NodeID, "modem", 'undefined') ->
    'undefined';
create_ota_command(Host, NodeID, "mcu", _ID) ->
    download_url(Host, "mcu", NodeID, "bin");
create_ota_command(Host, NodeID, "modem", _ID) ->
    download_url(Host, "modem", NodeID, "zip").

%%--------------------------------------------------------------------
%% @doc
%% Get MCU firmware id
%% @end
%%--------------------------------------------------------------------
-spec get_mcu_id(any()) -> string().
get_mcu_id([]) -> "undefined";
get_mcu_id([{"mcu", ID}|_]) -> ID;
get_mcu_id([_|Firmwares]) ->
    get_mcu_id(Firmwares).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec publish_status(boolean(), string(), string(), string(), map() | string()) -> 'ok'.
publish_status(Success, JobID, NodeID, FirmwareID, Status) when is_list(Status) ->
    publish_status(Success, JobID, NodeID, FirmwareID, #{"status" => Status});
publish_status(Success, JobID, "unknown", FirmwareID, Update) when is_map(Update) ->
    Map1 = #{
        "firmwareid" => FirmwareID
        ,"jobid" => JobID
        ,"success" => Success
        ,"name" => "OTAStatus"
        ,"when" => legacy_helpers:time()
    },
    Map2 = maps:merge(Map1, Update),
    lager:debug("publishing status update ~p", [Map2]),
    rabbit_helper:publish(
        <<"node.events">>
        ,<<(legacy_helpers:symbol_to_binary(JobID))/binary, ".ota.status">>
        ,msgpack:pack(Map2)
    );
publish_status(Success, JobID, NodeID, FirmwareID, Update) when is_map(Update) ->
    Map1 = #{
        "firmwareid" => FirmwareID
        ,"nodeid" => NodeID
        ,"jobid" => JobID
        ,"success" => Success
        ,"name" => "OTAStatus"
        ,"when" => legacy_helpers:time()
    },
    Map2 = maps:merge(Map1, Update),
    lager:debug("publishing status update ~p", [Map2]),
    rabbit_helper:publish(
        <<"node.events">>
        ,<<(legacy_helpers:symbol_to_binary(JobID))/binary, ".ota.status">>
        ,msgpack:pack(Map2)
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_firmware_from_directory(string(), string(), string()) -> {'ok', [string(), ...]} | {'error', atom()}.
get_firmware_from_directory(FirmwareID, Version, Directory) ->
    lager:debug("looking into ~p", [Directory]),
    FullDir = string:join([?STORAGE_DIR, Directory], "/"),
    {'ok', FirmwaresFiles} = file:list_dir(FullDir),
    lager:debug("found ~p with firmware ID ~p", [FirmwaresFiles, FirmwareID]),
    case [string:join([FullDir, F], "/") || F <- legacy_helpers:filter_by(FirmwaresFiles, Version)] of
        [] -> {'error', 'no_file_for_version'};
        Files -> {'ok', Files}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_id_from_file(string()) -> string().
get_id_from_file(Path) ->
    Tokens = string:tokens(Path, "/"),
    Name = lists:last(Tokens),
    [_, _Tag, _, _Version, IDExt] = string:tokens(Name, "-"),
    [ID, _Ext] = string:tokens(IDExt, "."),
    ID.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec default_args(string(), string(), string(), string() ,string()) -> map().
default_args(Host, JobID, Version, NodeID, FirmwareID) ->
    #{
        "jobid" => JobID
        ,"version" => Version
        ,"nodeid" => NodeID
        ,"retry" => ?RETRY_POLICY
        ,"host" => Host
        ,"firmwares" => [{"mcu", FirmwareID}]
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec modem_args(string(), string(), string(), string(), string(), string()) -> map().
modem_args(Host, JobID, Version, NodeID, FirmwareID, ModemID) ->
    #{
        "jobid" => JobID
        ,"version" => Version
        ,"nodeid" => NodeID
        ,"retry" => ?RETRY_POLICY
        ,"host" => Host
        ,"firmwares" => [
            {"mcu", FirmwareID}
            ,{"modem", ModemID}
        ]
    }.

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

download_url_test() ->
    Result = download_url("127.0.0.1", "modem", "N_1", "zip"),
    ?assertEqual(<<"http://127.0.0.1:8080/ota/modem/N_1.zip">>, Result),
    Result1 = download_url("127.0.0.1", "mcu", "N_1", "bin"),
    ?assertEqual(<<"https://127.0.0.1:10443/ota/mcu/N_1.bin">>, Result1),
    'ok'.

-endif.
