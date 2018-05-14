%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% Starts Legacy top supervisor (`leagcy_sup').
%% @end
%%%-------------------------------------------------------------------
-module(legacy_app).

-behaviour(application).

-include("legacy.hrl").

-define(HTTP_LISTENER, 'legacy_http').
-define(HTTPS_LISTENER, 'legacy_https').

%% Application callbacks
-export([
    start/2
    ,stop/1
]).

%%====================================================================
%% API
%%====================================================================
start(_StartType, _StartArgs) ->
    lager:info("starting Legacy"),
    'ok' = filelib:ensure_dir(?STORAGE_DIR ++ "/"),
    legacy_sup:start_link().

stop(_State) ->
    lager:warning("LEGACY stopped"),
    'ok'.

%%====================================================================
%% Internal functions
%%====================================================================
