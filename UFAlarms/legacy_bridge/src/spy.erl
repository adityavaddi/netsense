-module(spy).
-compile(export_all).
get_legacy_queue() ->
        io:format("Time:~p~n",[erlang:localtime()]),
        Lb = 'legacy_bridge@127.0.0.1',
        Pid = rpc:call(Lb, erlang, whereis,[legacy_zmq_client]),
        io:format("Legacy_zmq_client PID is:~p~n",[Pid]),
        rpc:call(Lb,erlang, process_info,[Pid, message_queue_len]).


get_queue(Proc) ->
        Lb = 'legacy_bridge@127.0.0.1',
        Pid = rpc:call(Lb,erlang, whereis,[Proc]),
        rpc:call(Lb,erlang, process_info,[Pid,message_queue_len]).

get_legacy_mem() ->
        io:format("Time:~p~n",[erlang:localtime()]),
        Lb = 'legacy_bridge@127.0.0.1',
        Pid = rpc:call(Lb, erlang, whereis,[legacy_zmq_client]),
        io:format("Legacy_zmq_client PID is:~p~n",[Pid]),
        rpc:call(Lb,erlang, process_info,[Pid, total_heap_size]).

get_this(Proc,Attr) ->
        Lb = 'legacy_bridge@127.0.0.1',
        Pid = rpc:call(Lb,erlang, whereis,[Proc]),
        rpc:call(Lb,erlang, process_info,[Pid,Attr]).


%rp([{-Reduc, Pid, case process_info(Pid, registered_name) of
%{registered_name,Name} -> Name; _ -> '_' end} ||
%    {Reduc, Pid} <-
%    lists:foldl(
%        fun(Pid, L) when length(L) > 40 ->
%               SL = lists:sublist(lists:keysort(1, L), 20),
%               case process_info(Pid, reductions) of
%                   {reductions,Reduc} -> [{-Reduc, Pid} | SL];
%                   undefined -> L
%               end;
%           (Pid, L) ->
%               case process_info(Pid, reductions) of
%                   {reductions,Reduc} -> [{-Reduc, Pid} | L];
%                   undefined -> L
%               end
%       end, [], erlang:processes())]).