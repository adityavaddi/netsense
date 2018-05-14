-module(legacy_fw_worker).

-author("Gina Hagg ghagg@sensity.com").

-export([send_to_nodes/4, unpack_protify_send/1]).

-compile([{parse_transform, lager_transform}]).

%%--------------------------------------------------------------------
%% @public
%%--------------------------------------------------------------------
unpack_protify_send(UnpackedMsgs) ->
    Nodeids = maps:get("nodeid", UnpackedMsgs),
    Version = maps:get("firmwareid", UnpackedMsgs),
    lager:info("Firmware update command for version: ~p for node ids: ~p~n", [Version, Nodeids]),
    FilesAreHere = do_s3_download(Version),
    send_to_nodes(FilesAreHere, Version, Nodeids, UnpackedMsgs).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------

send_to_nodes('true', Version, Nodeids, _UnpackedMsgs) ->
    lager:debug("send_to_nodes:we got ~p~n", [Nodeids]),
    {'ok', DownloadHost} = application:get_env('legacy_bridge', 'ota'),
    FirmwareId = firmware_helper:firmware_id_from_version(Version),
    [send_to_nodes_1(V1, DownloadHost, FirmwareId, Version) || V1 <- Nodeids];
send_to_nodes('false', Version, Nodeids, _UnpackedMsgs) ->
    lager:warning("~p is not in the server repo and could not downloaded from S3", [Version]),
    lager:warning("firmware update failed for these nodes ~p", [Nodeids]).

send_to_nodes_1(Nodeid, DownloadHost, FirmwareId, Version) ->
    Url = erlang:iolist_to_binary(["https://", DownloadHost,
       ":10443/device/", Nodeid, "/", FirmwareId,
       "/", Version, ".bin"]),
    lager:debug("sending ~p to node ~p", [Url, Nodeid]),
    UploadReq = {'SoftwareUpdateReq', Url},
    ProtoBinary = unode_proto_handler:encode_msgs([UploadReq]),
    lager:info("proto encoded command: ~p", [ProtoBinary]),
    subscriber_helper:send_to_nodes(ProtoBinary, "SoftwareUploadReq", [Nodeid], []).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
do_s3_download(Version) ->
    FirmwareId = firmware_helper:firmware_id_from_version(Version),
    case FirmwareId of
        [] ->
            lager:notice("Wrongly formatted Version ~p in api call. Format is <<nodetype-firmwareid>>, i.e v4-c456ef", [Version]);
        _ ->
        DirName = firmware_helper:firmware_directory(FirmwareId),
        case filelib:is_dir(DirName) of
            'true' ->
                lager:info("~p is in the server repository.", [FirmwareId]),
                'true';
            'false' ->
                lager:info("~p is not in server repository, downloading it from s3", [FirmwareId]),
                Files = legacy_s3_server:download_firmware(Version),
                lager:info("server downloaded files ~p", [Files]),
                case Files of
                    [] -> 'false';
                    _ -> 'true'
                end
        end
    end.
