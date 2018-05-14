-module(queue_observer).
  
-behaviour(gen_server).
 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-export([start_link/0]).

-compile([{parse_transform, lager_transform}]).

-define (PROCS,[zmqclientdealer]).


start_link() -> lager:debug("starting queue_observer"), gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Timer = erlang:send_after(60000, self(), check),
  {ok, Timer}.

handle_info(check, OldTimer) ->
  erlang:cancel_timer(OldTimer),
  check_local_queues(),
  Timer = erlang:send_after(300000, self(), check),
  {noreply, Timer}.
 
handle_call(_Msg, _From, State) ->
  {noreply,State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

check_local_queue(Proc) ->
   Pid = erlang:whereis(Proc),
   Quelen = erlang:process_info(Pid,message_queue_len),
   lager:debug("This ~p(~p) has ~p messages in queue~n",[Proc, Pid, Quelen]),
   TopOffenders = recon:proc_window(reductions, 5, 100),
   lager:debug("Top 5 offenders are ~p~n",[TopOffenders]).

check_local_queues() ->
    [check_local_queue(Proc) || Proc <- ?PROCS].