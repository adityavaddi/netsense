%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Legacy OTA handler ==
%% Handle HTTP req from node to download firmware files.
%% @end
%%%-------------------------------------------------------------------
-module(legacy_ota_handler).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    init/2
    ,allowed_methods/2
    ,resource_exists/2
    ,content_types_provided/2
    ,handle/2
    ,terminate/3
]).

-include("legacy.hrl").
-include_lib("kernel/include/file.hrl").

-record(state, {
    nodeid
    ,type
    ,firmwareid
    ,version
    ,ext
    ,file
}).

-type state() :: #state{}.
-type req() :: cowboy_req:req().

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Init request
%% @end
%%--------------------------------------------------------------------
-spec init(req(), any()) -> {'cowboy_rest', req(), state()}.
init(Req, _Opts) ->
    Type = cowboy_req:binding('type', Req),
    NodeIDExt = legacy_helpers:symbol_to_string(cowboy_req:binding('node_id', Req)),
    [NodeID, Ext] = string:tokens(NodeIDExt, "."),
    State = #state{
        type=legacy_helpers:symbol_to_string(Type)
        ,nodeid=legacy_helpers:symbol_to_string(NodeID)
        ,ext="." ++ Ext
    },
    legacy_helpers:init_lager(NodeID),
    lager:info("node ~p trying to download ~p.~p", [NodeID, Type, Ext]),
    {'cowboy_rest', Req, State}.

%%--------------------------------------------------------------------
%% @doc
%% Only allow `GET' request
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(req(), state()) -> {[binary(), ...], req(), state()}.
allowed_methods(Req, State) ->
	{[<<"GET">>], Req, State}.

%%--------------------------------------------------------------------
%% @doc
%% Check if file requested exists
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(req(), state()) -> {boolean(), req(), state()}.
resource_exists(Req, #state{nodeid=NodeID, type=Type, ext=Ext}=State) ->
    case legacy_ota_worker:get_info(NodeID) of
        {'error', _Reason} ->
            lager:warning("node ~p is not registered for OTA update: ~p", [NodeID, _Reason]),
            {'false', Req, State};
        {'ok', Map} ->
            Version = maps:get("version", Map),
            Firmwares = maps:get("firmwares", Map),
            FirmwareID = legacy_ota_helpers:get_mcu_id(Firmwares),
            Filter =
                case Type =:= "modem" of
                    'true' -> "modem";
                    'false' -> Ext
                end,
            case legacy_ota_helpers:get_firmware(FirmwareID, Version) of
                {'error', _Reason} ->
                    lager:warning("failed to get firmware ~p for version ~p", [FirmwareID, Version]),
                    {'false', Req, State#state{firmwareid=FirmwareID, version=Version}};
                {'ok', Files} ->
                    case legacy_helpers:filter_by(Files, Filter) of
                        [] ->
                            lager:warning("failed to get firmware ~p with version ~p & ext ~p", [FirmwareID, Version, Filter]),
                            {'false', Req, State#state{firmwareid=FirmwareID, version=Version}};
                        [File] ->
                            {'true', Req, State#state{file=File, firmwareid=FirmwareID, version=Version}};
                        Files ->
                            lager:warning("failed to get firmware ~p, too many files ~p", [FirmwareID, Files]),
                            {'false', Req, State#state{firmwareid=FirmwareID, version=Version}}
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(req(), state()) -> {any(), req(), state()}.
content_types_provided(Req, State) ->
	{[{<<"text/html">>, 'handle'}], Req, State}.

%%--------------------------------------------------------------------
%% @doc
%% Send file to client and update OTA status
%% @end
%%--------------------------------------------------------------------
-spec handle(req(), state()) -> {any(), req(), state()}.
handle(Req, #state{file=File, nodeid=NodeID, firmwareid=FwID}=State) ->
    lager:debug("downloading file ~p", [File]),
    erlang:spawn('legacy_ota_worker', 'handle_file_download', [NodeID, ?OTA_START_DOWNLOAD, FwID, File]),
    {'ok', #file_info{size=Size}} = file:read_file_info(File),
	{{'sendfile', 0, Size, File}, Req, State}.

%%--------------------------------------------------------------------
%% @doc
%% Called when request ternimated and update OTA status
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), req(), state()) -> 'ok'.
terminate(Reason, _Req, #state{file='undefined'}) ->
    lager:debug("terminated because: ~p", [Reason]),
    'ok';
terminate(Reason, _Req, #state{file=File, nodeid=NodeID, firmwareid=FwID}) ->
    lager:debug("terminated because: ~p", [Reason]),
    erlang:spawn('legacy_ota_worker', 'handle_file_download', [NodeID, ?OTA_STOP_DOWNLOAD, FwID, File]),
    'ok'.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
