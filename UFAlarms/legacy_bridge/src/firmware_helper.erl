-module(firmware_helper).

-compile([{parse_transform, lager_transform}]).

-export([firmware_directory/1,
	 firmware_id_from_version/1, firmwares_directory/0,
	 node_version_from_version/1]).

-define(DEFAULT_FW_DIR, "/sensity_firmware/").

%%--------------------------------------------------------------------
%% @public
%%--------------------------------------------------------------------
-spec firmwares_directory() -> string().
firmwares_directory() ->
    {ok, [[Home]]} = init:get_argument(home),
    FwDir = application:get_env(legacy_bridge, fwdir,
				?DEFAULT_FW_DIR),
    string:join([Home, FwDir], "/").

%%--------------------------------------------------------------------
%% @public
%%--------------------------------------------------------------------
-spec firmware_directory(string()) -> string().
firmware_directory(FwId) ->
    FwDir = firmwares_directory(),
    {ok, Dirs} = file:list_dir(FwDir),
    Files = [V1
	     || V1 <- Dirs, firmware_directory_1(V1, FwId)],
    case Files of
      [] -> [];
      [File | _] -> string:join([FwDir, File], "/")
    end.

firmware_directory_1(Dir, FwId) ->
    case string:str(Dir, FwId) of
      0 -> false;
      _ -> true
    end.

%%--------------------------------------------------------------------
%% @public
%%--------------------------------------------------------------------
-spec node_version_from_version(string()) -> string().
node_version_from_version(Version) ->
    [NodeVersion, _] = string:tokens(Version, "-"),
    NodeVersion.

%%--------------------------------------------------------------------
%% @public
%%--------------------------------------------------------------------
-spec firmware_id_from_version(string()) -> string().
firmware_id_from_version(Version1) ->
    Version = case is_binary(Version1) of true -> binary_to_list(Version1); _-> Version1 end,
    Tokens = string:tokens(Version, "-"),
    case Tokens of [_, FirmwareId] -> FirmwareId;
      _-> lager:notice("Firmware id you entered is not correct, Firmware id is in <<nodetype-firmwarenumber>>, i.e v4-c45699f",[Version]),
        []
    end.
