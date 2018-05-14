-module(agent_group_handler).
  
-behaviour(gen_server).
 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-export([add_group/2, lookup_group/1, delete_group/1, start_link/0, purge/0]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
add_group(Groupname, Nodeids) -> gen_server:call(?MODULE, {add_group, Groupname, Nodeids}).
lookup_group(Groupname) -> gen_server:call(?MODULE, {lookup_group, Groupname}).
delete_group(Groupname) -> gen_server:call(?MODULE, {delete_group, Groupname}).
purge() -> gen_server:call(?MODULE,{purge}).

%Args = [<0.8325.0>]
init(_Args) ->
  Tab = ets:new(?MODULE, [set,private]),
  {ok,Tab}.
 
handle_call({add_group, Groupname, Nodeids}, _From, Tab) ->
  lager:info("agent_group_handler: add group is called: group: ~s, ~p~n", [Groupname, Nodeids]),
   Response = case ets:member(Tab, Groupname) of
     false -> 
          lager:info("agent_group_handler: is empty, will add group: ~p~n",[Groupname]),
           ets:insert_new(Tab, {Groupname, Nodeids}),
           ok;
     true ->
           lager:info("agent_group_handler:add_group, there is a group: we will update it.~n"),
           ets:insert(Tab,{Groupname, Nodeids})
   end,
   {reply, Response, Tab};

handle_call({delete_group, Groupname}, _From, Tab) ->
  lager:info("agent_group_handler: add group is called: group: ~s~n", [Groupname]),
   Resp = ets:delete(Tab, Groupname),
   {reply, Resp, Tab};
 
handle_call({lookup_group, Groupname}, _From, Tab) ->
    Reply = ets:lookup(Tab, Groupname),        
    {reply, Reply, Tab};
 
handle_call({purge}, _From, Tab) -> ets:delete_all_objects(Tab), {reply, ok, Tab}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.