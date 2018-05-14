-module(legacy_db_server).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([install/0, add_node/5, add_node_dirty/5,lookup_node/1, update_node/1, add_connected/1, delete_connected/1,
	fetch_site_org/1, read_node_fun/1]).

-record(state, {}).
%-record(nodes, {nodeid, org, site, group, swid}).
-define(TAB, 'nodes').
-define(CONNTAB, 'connected_nodes').
-include_lib("stdlib/include/qlc.hrl").
-include("legacy.hrl").
-compile([{parse_transform, lager_transform}]).
%% API.


-spec start_link() -> {'ok', pid()}.
start_link() ->
	lager:info("legacy_db_server starting"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.
install() -> gen_server:call(?MODULE, {install}).
add_node_dirty(Nodeid, Swid, Org, Site, Group) -> gen_server:call(?MODULE, {'add_node_dirty', Nodeid, Swid, Org, Site, Group}).
add_node(Nodeid, Swid, Org, Site, Group) -> gen_server:call(?MODULE, {'add_node', Nodeid, Swid, Org, Site, Group}).
lookup_node(Key) -> gen_server:call(?MODULE, {'lookup_node', Key}, 'infinity').
update_node(Node) -> gen_server:call(?MODULE,{'update_node',Node}).
add_connected(Node) -> gen_server:call(?MODULE, {'add_connected', Node}).
delete_connected(Node) -> gen_server:call(?MODULE, {'delete_connected', Node}).
fetch_site_org(Key) -> gen_server:call(?MODULE,{'fetch_site_org', Key}, 'infinity').

init(_Args) ->
    self() ! 'post_init',
	{ok, #state{}}.

handle_call({'update_existing', Key, LoginMap}, _From, State) ->
    Keyy = case is_binary(Key) of 'true' -> Key; _-> list_to_binary(Key) end,
	[Info] = mnesia:dirty_read({'nodes', Keyy}),
	NewInfo = Info#nodes{loginmap = LoginMap},
	lager:debug("updateexisting:Node lookup returned: ~p~n",[Info]),
	Result = mnesia:dirty_write(NewInfo),
    {reply, Result, State};

handle_call({'add_connected', Nodeid} , _From, State) ->
    lager:debug("add_connected is called"),
	Result = mnesia:dirty_write(#connected_nodes{nodeid = Nodeid}),
	{reply, Result, State};

handle_call({'delete_connected', Nodeid} , _From, State) ->
    lager:debug("delete_connected is called"),
	Result = mnesia:dirty_delete(#connected_nodes{nodeid = Nodeid}),
	{'reply', Result, State};

%mnesia:write(P#nodes{nodeid = Node, site = Site}),
handle_call({'update_node', Node} , _From, State) ->
    lager:debug("update_node is called"),
	Result = mnesia:dirty_write(Node),
	{'reply', Result, State};

handle_call({'add_node', Nodeid, Swid, Org, Site, Group} , _From, State) ->
    lager:debug("addnode is called"),
    Node = #nodes{nodeid = Nodeid, swid = Swid, org = Org, site = Site, group=Group},
    Fun = fun() -> mnesia:write(Node) end,
    Result = mnesia:transaction(Fun),
	{'reply', Result, State};

%[{nodes,"N01232ea5","unknown","unknown","unknonwn","unknown"}]
handle_call({'lookup_node', Key}, _From, State) ->
    Keyy = case is_binary(Key) of 'true' -> Key; _-> list_to_binary(Key) end,
	Info = mnesia:dirty_read({?TAB, Keyy}),
	%ets:match_object(Tab, #nodes{nodeid=Keyy, _='_'}),
	lager:debug("Node lookup returned: ~p~n",[Info]),
    {'reply', Info, State};

 %[{nodes,"N01232ea5","unknown","unknown","unknonwn","unknown"}]
handle_call({'fetch_site_org', Key}, _From, State) ->
    Keyy = case is_binary(Key) of 'true' -> Key; _-> list_to_binary(Key) end,
        {Site,Org}  =  case catch mnesia:dirty_read(?TAB, Keyy) of
        {'EXIT',_} ->
                  lager:debug("must be not exists, mnesia abort error" ),
                    {[],[]};
        [] -> {[],[]};
             Nodeinfo   -> [Node] = Nodeinfo,
                    {Node#nodes.org,
                        Node#nodes.site}
        end,
        lager:debug("Siteid ~p, Orgid ~p for node is ~p~n",[Site, Org, Keyy]),
    {'reply', {Org,Site}, State};

 handle_call({'install'}, _From , State) ->
    Result = create_tables(),
	{'reply', Result, State};

handle_call(_Request, _From, State) ->
	{'noreply', State}.

handle_cast(_Msg, State) ->
	{'noreply', State}.

handle_info('post_init', State) ->
	lager:info("legacy_db:post_init"),
	create_tables(),
	{'noreply', State};

handle_info(_Info, State) ->
	{'noreply', State}.

terminate(_Reason, _State) ->
	'ok'.

table_exists(TableName) ->
    Tables = mnesia:system_info('tables'),
    lists:member(TableName,Tables).

code_change(_OldVsn, State, _Extra) ->
	{'ok', State}.

create_tables() ->
    lager:debug("Mnesia:info ~p~n", [mnesia:info()]),
	lager:debug("checking to see if tables exist"),
	mnesia:change_table_copy_type('schema', node(), 'disc_copies'),
    R1 = case table_exists('nodes') of 'false' ->
		mnesia:create_table('nodes',[{'disc_copies', [node()]},{'attributes', record_info('fields', 'nodes')}]);
	_-> {'ok', 'already_exists'} end,
	{R1}.

read_node_fun(Node) ->
    fun() ->
    	    mnesia:read({'nodes', Node})
    end.
