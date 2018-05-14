-module(dcc_db).
-author("Gina Hagg <ghagg@sensity.com").
-behaviour(gen_server).

-export([addConfig/3, readConfig/2, getKeys/0]).

-export([start_link/0, init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-record(state, {}).
-record(idkey, {nodeid =[],uuid =[]}).
-record(video_nodes, {idkey, config = []}).
-define(VIDEOTAB, video_nodes).
-compile([{parse_transform, lager_transform}]).
%%----------------
%% public
%%----------------


addConfig(Nodeid, Uuid, Config) ->
    gen_server:call(?MODULE, {'addConfig', Nodeid, Uuid, Config}).

readConfig(Nodeid, Uuid) ->
    gen_server:call(?MODULE, {'readConfig', Nodeid, Uuid}).

getKeys() ->
    gen_server:call(?MODULE, {getKeys}).

%%----------------
%% private
%%----------------

%%----------------
%% server
%%----------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:create_table(video_nodes,[{disc_copies, [node()]},{attributes, record_info(fields, video_nodes)}]),
    {ok, #state{}}.

handle_call({addConfig, Nodeid, Uuid, Config}, _From , State) ->
    TabKey =  #idkey{nodeid = Nodeid, uuid = Uuid},
    Result = mnesia:dirty_write(video_nodes, #video_nodes{idkey =TabKey, config = Config}),
    lager:debug("Add Config returned: ~p~n",[Result]),
    {reply, Result, State};

handle_call(getKeys, _From , State) ->
    Result = mnesia:dirty_all_keys(video_nodes),
    lager:debug("all_keys returned: ~p~n",[Result]),
    {reply, Result, State};

handle_call({readConfig, Nodeid, Uuid}, _From , State) ->
    Idkey = #idkey{nodeid = Nodeid, uuid = Uuid},
    Result = mnesia:dirty_read(video_nodes, Idkey),
    lager:debug("Read Config: ~p~n",[Result]),
    {reply, Result, State};

handle_call({install}, _From , State) ->
    Result = create_tables(),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

table_exists(TableName) ->
    Tables = mnesia:system_info(tables),
    lists:member(TableName,Tables).

create_tables() ->
    lager:info("Mnesia:info ~p~n", [mnesia:info()]),
    lager:info("checking to see if tables exist"),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    R1 = case table_exists(video_nodes) of false ->
        mnesia:create_table(video_nodes,[{disc_copies, [node()]},{attributes, record_info(fields, video_nodes)}]);
    _->{ok, already_exists} end,
    {R1}.