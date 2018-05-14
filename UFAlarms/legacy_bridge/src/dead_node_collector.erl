-module(dead_node_collector).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0]).

-compile([{parse_transform, lager_transform}]).

start_link() ->
    lager:info("starting dead_node_collector"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    pg2:create('zmq_jt_group'),
    pg2:create('zmq_lt_group'),
    pg2:create('zmq_pp_group'),
    pg2:create('zmq_other_group'),
    pg2:create('listener_group'),
    pg2:create('distributor_group'),
    Timer = erlang:send_after(300000, self(), 'check'),
    {'ok', Timer}.

handle_info('check', OldTimer) ->
    erlang:cancel_timer(OldTimer),
    check_dead_nodes(),
    Timer = erlang:send_after(360000, self(), 'check'),
    {'noreply', Timer}.

handle_call(_Msg, _From, State) ->
    {'noreply', State}.

handle_cast(_Msg, State) ->
    {'noreply', State}.

terminate(_Reason, _State) -> 'ok'.

code_change(_OldVersion, State, _Extra) ->
    {'ok', State}.

check_dead_nodes() ->
    Pids = gproc:lookup_pids({'p', 'l',  'keepalive'}),
    Start = os:system_time('milli_seconds'),
    lists:map(
        fun(Pid) ->
            lager:debug("sending sensorsamplereq sensor gg which is non-existent. Pid: ~p~n ",[Pid]),
            Pid ! {'are_you_there'}
        end
        ,Pids
    ),
    End = os:system_time('milli_seconds'),
    lager:debug("the queue took ~p milliseconds to execute~n", [End-Start]).
