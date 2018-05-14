%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Legacy S3 Worker ==
%% Allow S3 file download
%% @end
%%%-------------------------------------------------------------------
-module(legacy_s3_worker).

-behavior(gen_server).

-include("legacy.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/1]).

-export([
    get_file_from_bucket/2
    ,get_file_from_bucket/3
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

-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({'local', ?SERVER}, ?SERVER, Args, []).

%%--------------------------------------------------------------------
%% @doc
%% Download file and return local path without options
%% @end
%%--------------------------------------------------------------------
-spec get_file_from_bucket(string(), string()) -> {'ok', string()} | {'error', atom()}.
get_file_from_bucket(Bucket, Name) ->
    get_file_from_bucket(Bucket, Name, []).

%%--------------------------------------------------------------------
%% @doc
%% Download file and return local path with options
%%
%% Options:
%% <ul>
%% <li>`{zip, true}' unzip downloaded file</li>
%% </ul>
%% @end
%%--------------------------------------------------------------------
-spec get_file_from_bucket(string(), string(), list()) -> {'ok', string()} | {'error', atom()}.
get_file_from_bucket(Bucket, Name, Opts) ->
    gen_server:call(?MODULE, {'get_file', Bucket, Name, Opts}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(Args) ->
    process_flag('trap_exit', 'true'),
    Key = proplists:get_value('key', Args),
    Secret = proplists:get_value('secret', Args),
    Host = proplists:get_value('host', Args),
    'ok' = erlcloud_s3:configure(Key, Secret, Host),
    lager:info("init with ~p", [Args]),
    {'ok', #state{}}.

handle_call({'get_file', Bucket, Name, Opts}, _From, State) ->
    Files = download_file(Bucket, Name, Opts),
    {'reply', Files, State};
handle_call(_Msg, _From, State) ->
    lager:debug("rcvd unknown call msg: ~p from: ~p", [_Msg, _From]),
    {'reply', 'ok', State}.

handle_cast(_Msg, State) ->
    lager:debug("rcvd unknown cast msg: ~p", [_Msg]),
    {'noreply', State}.

handle_info(_Msg, State) ->
    lager:debug("rcvd unknown info msg: ~p", [_Msg]),
    {'noreply', State}.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

terminate(_Reason, _State) ->
    lager:error("~p terminated: ~p", [?MODULE, _Reason]),
    'ok'.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec download_file(string(), string(), list()) -> {'ok', string()} | {'error', atom()}.
download_file(Bucket, Name, Opts) ->
    Unzip = proplists:get_value('unzip', Opts, 'false'),
    case erlcloud_s3:check_bucket_access(Bucket) of
        {'error', _Reason} ->
            lager:warning("failed checking bucket ~p : ~p", [Bucket, _Reason]),
            {'error', 'bucket_not_found'};
        'ok' ->
            case list_files(Bucket, Name) of
                [File|[]] ->
                    lager:debug("found ~p", [File]),
                    Content = fetch_file_content(Bucket, File),
                    {'ok', store_file(File, Content, Unzip)};
                [] ->
                    lager:warning("did not find any file matching ~p in bucket ~p", [Name, Bucket]),
                    {'error', 'file_not_found'};
                _Files ->
                    lager:warning("found too many files matching ~p in bucket ~p. Files: ~p", [Name, Bucket, _Files]),
                    {'error', 'too_many_files'}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec list_files(string(), string()) -> [string(), ...] | [].
list_files(Bucket, Name) ->
    Objects = erlcloud_s3:list_objects(Bucket),
    lists:filtermap(
        fun(Content) ->
            Key = proplists:get_value('key', Content, ""),
            case legacy_helpers:str_match(Key, Name) of
               'true' -> {'true', Key};
               'false' -> 'false'
            end
        end
        ,proplists:get_value('contents', Objects, [])
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_file_content(string(), string()) -> binary().
fetch_file_content(Bucket, File) ->
    lager:debug("fetching file: ~p", [File]),
    FileProps = erlcloud_s3:get_object(Bucket, File),
    proplists:get_value('content', FileProps).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec store_file(string(), binary(), boolean()) -> string().
store_file(File, Content, 'false') ->
    FilePath = string:join([?STORAGE_DIR, File], "/"),
    lager:debug("storing file here: ~p", [FilePath]),
    'ok' = file:write_file(FilePath, Content),
    FilePath;
store_file(File, Content, 'true') ->
    FilePath = string:join([?STORAGE_DIR, File], "/"),
    lager:debug("storing file here: ~p", [FilePath]),
    'ok' = file:write_file(FilePath, Content),
    lager:debug("unzipping file ~p in ~p", [FilePath, ?STORAGE_DIR]),
    {'ok', _Files} = zip:extract(FilePath, [{'cwd', ?STORAGE_DIR}]),
    'ok' = file:delete(FilePath),
    legacy_helpers:symbol_to_string(
        binary:replace(
            legacy_helpers:symbol_to_binary(File)
            ,<<".zip">>
            ,<<>>
        )
    ).
