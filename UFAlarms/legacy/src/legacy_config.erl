%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == Legacy Config ==
%% Config helpers for legacy
%% @end
%%%-------------------------------------------------------------------
-module(legacy_config).

-include("legacy.hrl").

-export([
    get/1
]).

%%--------------------------------------------------------------------
%% @doc
%% Fetch config from sys.config for legacy app.
%% @end
%%--------------------------------------------------------------------
-spec get(any()) -> 'undefined' | {'ok', any()}.
get(Key) ->
    application:get_env(?LEGACY, Key).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
