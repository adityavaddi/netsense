-module(application_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("messages_pb.hrl").

-export([
    all/0
    ,groups/0
    ,init_per_suite/1
    ,end_per_suite/1
]).

-export([
    application_started/1
]).

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
    [{'group', 'application'}].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Tests groups
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{'application', [], ['application_started']}].


%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special init config for suite
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ct:pal("START OF ~p", [?MODULE]),
    test_helpers:init_legacy(),
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
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Check if legacy application is started
%% @end
%%--------------------------------------------------------------------
application_started(_Config) ->
    test_helpers:mock_deps(),
    {'ok', _} = application:ensure_all_started('legacy'),
    Apps = application:which_applications(),
    {'legacy', _, _} = lists:keyfind('legacy', 1, Apps).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
