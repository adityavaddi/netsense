%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == RabbitMQ Event ==
%% Forward event to subscribers
%% @end
%%%-------------------------------------------------------------------
-module(rabbit_feed).

-behavior(gen_event).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([
    init/1
    ,handle_call/2
    ,handle_event/2
    ,handle_info/2
    ,terminate/2
    ,code_change/3
]).

-record(state, {pid}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([Pid]) ->
    {'ok', #state{pid=Pid}}.

handle_event(Event, #state{pid=Pid}=State) ->
    Pid ! {'rabbit_feed', Event},
    {'ok', State}.

handle_call(_Msg, State) ->
    lager:debug("rcv unhandled msg ~p", [_Msg]),
    {'ok', 'ok', State}.

handle_info(_Msg, State) ->
    lager:debug("rcv unhandled msg ~p", [_Msg]),
    {'ok', State}.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

terminate(_Reason, _state) ->
    lager:warning("terminating ~p", [_Reason]),
    'ok'.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
