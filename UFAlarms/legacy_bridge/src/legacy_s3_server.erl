-module(legacy_s3_server).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([download_firmware/1, start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([
    code_change/3
    ,handle_call/3
    ,handle_cast/2
    ,handle_info/2
    ,init/1
    ,terminate/2
]).

-define(DEFAULT_HOST, "s3-us-west-2.amazonaws.com").
-define(DEFAULT_KEY, "AKIAJHXVVJ2KABYSEUTA").
-define(DEFAULT_SECRET, "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").
-define(DEFAULT_BUCKET, "sensity-firmware").

-record(state, {config, bucket = [], s3host = [], fwsdir = []}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() -> {'ok', pid()}.
start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

download_firmware(Version) when is_binary(Version) ->
    gen_server:call(?MODULE, {'download_firmware', binary_to_list(Version)});
download_firmware(Version) ->
    gen_server:call(?MODULE, {'download_firmware', Version}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([]) ->
    application:ensure_started('ssl'),
    application:ensure_started('lhttpc'),
    erlcloud:start(),
    application:load('sasl'),
    application:set_env('sasl', 'sasl_error_logger', 'false'),
    self() ! 'post_init',
    {ok, #state{}}.

handle_call({'download_firmware', Version}, _From, #state{fwsdir = FwsDir, bucket = Bucket} = State) ->
    [NodeVersion, FirmwareVersion] = string:tokens(Version, "-"),
    Files = get_s3_firmware(NodeVersion, FirmwareVersion, FwsDir, Bucket),
    {'reply', Files, State};
handle_call(_Request, _From, State) ->
    {'reply', 'ignored', State}.

handle_cast(_Msg, State) -> {'noreply', State}.

handle_info(post_init, State) ->
    S3Host = application:get_env('legacy_bridge', 's3host', ?DEFAULT_HOST),
    S3Bucket = application:get_env('legacy_bridge', 's3bucket', ?DEFAULT_BUCKET),
    FwsDir = firmware_helper:firmwares_directory(),
    AwsKey = application:get_env('legacy_bridge', 'aws_key', ?DEFAULT_KEY),
    SecretKey = application:get_env('legacy_bridge', 'aws_secret', ?DEFAULT_SECRET),
    erlcloud_s3:configure(AwsKey, SecretKey, S3Host),
    lager:info("init with host: ~p with key: ~p", [S3Host, AwsKey]),
    lager:info("init using bucket: ~p and fwdir: ~p", [S3Bucket, FwsDir]),
    {'noreply', State#state{s3host = S3Host, bucket = S3Bucket, fwsdir = FwsDir}};
handle_info(_Info, State) ->
    {'noreply', State}.

terminate(_Reason, _State) -> 'ok'.

code_change(_OldVsn, State, _Extra) -> {'ok', State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
get_s3_firmware(NodeVersion, FirmwareId, FwsDir, Bucket) ->
    lager:info("getting firmware version: ~p for a ~p node", [FirmwareId, NodeVersion]),
    case list_files(Bucket, FirmwareId) of
        [] ->
            lager:warning("did not find any firmware with version: ~p", [FirmwareId]),
            [];
        [File | _] ->
            {File, Content} = fetch_file(File),
            StoredFilePath = store_file(File, Content, FwsDir),
            Files = unzip_file(StoredFilePath),
            select_file(Files, NodeVersion, FirmwareId)
    end.

list_files(Bucket, FirmwareId) ->
    Data = erlcloud_s3:list_objects(Bucket),
    lists:filtermap(
        fun(File) ->
            Name = proplists:get_value('key', File),
            case match(Name, FirmwareId) of
                'true' -> {'true', Name};
                Else -> Else
            end
        end,
        proplists:get_value(contents, Data, [])).

fetch_file(File) ->
    lager:debug("fetching file: ~p", [File]),
    FileProps = erlcloud_s3:get_object(?DEFAULT_BUCKET, File),
    Content = proplists:get_value('content', FileProps),
    {File, Content}.

store_file(File, Content, FwsDir) ->
    FilePath = string:join([FwsDir, File], "/"),
    lager:debug("storing file: ~p", [FilePath]),
    filelib:ensure_dir(FilePath),
    Checksum = checksum(Content),
    lager:debug("checksum ~p for File: ~p",
    [Checksum, FilePath]),
    'ok' = file:write_file(FilePath, Content),
    FilePath.

unzip_file(FilePath) ->
    UnzipPath = firmware_helper:firmwares_directory(),
    filelib:ensure_dir(UnzipPath),
    lager:debug("unzipping file ~p in ~p", [FilePath, UnzipPath]),
    {'ok', Files} = zip:extract(FilePath, [{'cwd', UnzipPath}]),
    Removed = file:delete(FilePath),
    lager:debug("removed archive: ~p", [Removed]),
    Files.

select_file(Files, NodeVersion, FirmwareId) ->
    Version = string:join([NodeVersion, FirmwareId], "-"),
    [V1 || V1 <- Files, select_file_1(V1, Version)].

select_file_1(File, Version) -> match(File, Version).

match('undefined', _Match) -> 'false';
match(Subject, Match) ->
    case string:str(Subject, Match) of
      0 -> 'false';
      _ -> 'true'
    end.

checksum(Data) -> checksum(iolist_to_binary(Data), 0).

checksum(<<>>, Acc) -> Acc;
checksum(<<I, T/binary>>, Acc) -> checksum(T, I + Acc).
