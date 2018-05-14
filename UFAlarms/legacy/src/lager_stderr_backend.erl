%% Copyright (c) 2011-2012, 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc Console backend for lager.
%% Configuration is a proplist with the following keys:
%% <ul>
%%    <li>`level' - log level to use</li>
%%    <li>`use_stderr' - either `true' or `false', defaults to true. If set to true,
%%                       use standard error to output console log messages</li>
%%    <li>`formatter' - the module to use when formatting log messages. Defaults to
%%                      `lager_default_formatter'</li>
%%    <li>`formatter_config' - the format configuration string. Defaults to
%%                             `time [ severity ] message'</li>
%% </ul>

-module(lager_stderr_backend).

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
        code_change/3]).

-record(state, {level :: {'mask', integer()},
                out = user :: user | standard_error,
                formatter :: atom(),
                format_config :: any(),
                colors=[] :: list()}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).
-endif.

-include_lib("lager/include/lager.hrl").

-define(TERSE_FORMAT,[time, " ", color, "[", severity,"] ", message]).
-define(DEFAULT_FORMAT_CONFIG, ?TERSE_FORMAT ++ [eol()]).
-define(FORMAT_CONFIG_OFF, [{eol, eol()}]).

-ifdef(TEST).
-define(DEPRECATED(_Msg), ok).
-else.
-define(DEPRECATED(Msg),
        io:format(user, "WARNING: This is a deprecated console configuration. Please use \"~w\" instead.~n", [Msg])).
-endif.

%% @private
init([Level]) when is_atom(Level) ->
    ?DEPRECATED([{level, Level}]),
    init([{level, Level}]);
init([Level, true]) when is_atom(Level) -> % for backwards compatibility
    ?DEPRECATED([{level, Level}, {formatter_config, [{eol, "\\r\\n\\"}]}]),
    init([{level, Level}, {formatter_config, ?FORMAT_CONFIG_OFF}]);
init([Level, false]) when is_atom(Level) -> % for backwards compatibility
    ?DEPRECATED([{level, Level}]),
    init([{level, Level}]);

init(Options) when is_list(Options) ->
    true = validate_options(Options),
    Colors = case application:get_env(lager, colored) of
        {ok, true} ->
            {ok, LagerColors} = application:get_env(lager, colors),
            LagerColors;
        _ -> []
    end,

    Level = get_option(level, Options, undefined),
    try {is_new_style_console_available(), lager_util:config_to_mask(Level)} of
        {false, _} ->
            Msg = "Lager's console backend is incompatible with the 'old' shell, not enabling it",
            %% be as noisy as possible, log to every possible place
            try
                alarm_handler:set_alarm({?MODULE, "WARNING: " ++ Msg})
            catch
                _:_ ->
                    error_logger:warning_msg(Msg ++ "~n")
            end,
            io:format("WARNING: " ++ Msg ++ "~n"),
            ?INT_LOG(warning, Msg, []),
            {error, {fatal, old_shell}};
        {true, L} ->
            [UseErr, Formatter, Config] = [ get_option(K, Options, Default) || {K, Default} <- [
                                                                                   {use_stderr, true},
                                                                                   {formatter, lager_default_formatter},
                                                                                   {formatter_config, ?DEFAULT_FORMAT_CONFIG}
                                                                                               ]
                                          ],
            Out = case UseErr of
                     false -> user;
                     true -> standard_error
                  end,
            {ok, #state{level=L,
                    out=Out,
                    formatter=Formatter,
                    format_config=Config,
                    colors=Colors}}
    catch
        _:_ ->
            {error, {fatal, bad_log_level}}
    end;
init(Level) when is_atom(Level) ->
    ?DEPRECATED([{level, Level}]),
    init([{level, Level}]);
init(Other) ->
    {error, {fatal, {bad_console_config, Other}}}.

validate_options([]) -> true;
validate_options([{level, L}|T]) when is_atom(L) ->
    case lists:member(L, ?LEVELS) of
        false ->
            throw({error, {fatal, {bad_level, L}}});
        true ->
            validate_options(T)
    end;
validate_options([{use_stderr, true}|T]) ->
    validate_options(T);
validate_options([{use_stderr, false}|T]) ->
    validate_options(T);
validate_options([{formatter, M}|T]) when is_atom(M) ->
    validate_options(T);
validate_options([{formatter_config, C}|T]) when is_list(C) ->
    validate_options(T);
validate_options([H|_]) ->
    throw({error, {fatal, {bad_console_config, H}}}).

get_option(K, Options, Default) ->
   case lists:keyfind(K, 1, Options) of
       {K, V} -> V;
       false -> Default
   end.

%% @private
handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    try lager_util:config_to_mask(Level) of
        Levels ->
            {ok, ok, State#state{level=Levels}}
    catch
        _:_ ->
            {ok, {error, bad_log_level}, State}
    end;
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log, Message},
    #state{level=L,out=Out,formatter=Formatter,format_config=FormatConfig,colors=Colors} = State) ->
    case lager_util:is_loggable(Message, L, ?MODULE) of
        true ->
            io:put_chars(Out, Formatter:format(Message,FormatConfig,Colors)),
            {ok, State};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

eol() ->
    case application:get_env(lager, colored) of
        {ok, true}  ->
            "\e[0m\r\n";
        _ ->
            "\r\n"
    end.

is_new_style_console_available() ->
    %% Criteria:
    %% 1. If the user has specified '-noshell' on the command line,
    %%    then we will pretend that the new-style console is available.
    %%    If there is no shell at all, then we don't have to worry
    %%    about log events being blocked by the old-style shell.
    %% 2. Windows doesn't support the new shell, so all windows users
    %%    have is the oldshell.
    %% 3. If the user_drv process is registered, all is OK.
    %%    'user_drv' is a registered proc name used by the "new"
    %%    console driver.
    init:get_argument(noshell) /= error orelse
        element(1, os:type()) /= win32 orelse
        is_pid(whereis(user_drv)).
