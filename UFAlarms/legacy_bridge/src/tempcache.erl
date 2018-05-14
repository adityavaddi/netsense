-module(tempcache).
-compile(export_all).
-compile([{parse_transform, lager_transform}]).

dets_new_cache(Tab) ->
    dets:open_file(Tab,[{access,read_write},{type, set}]).
    
dets_save_detail(Tab,Key, Detail) ->
    {ok, Tabl} = dets_new_cache(Tab),
    dets:insert(Tabl, {Key, Detail}),
    dets_close(Tabl),
    {ok,Detail}.

dets_lookup_key(Tab,Key) ->
    {ok, Tabl} = dets_new_cache(Tab),
    Val = case dets:lookup(Tabl, Key) of 
    [{Key, Value}] -> Value;
    _-> []
    end,
    dets_close(Tab),
    Val.

dets_check_save(Tab, Key,Detail)->
    Val = case dets_lookup_key(Tab,Key) of
        [] ->
        {ok, Det} = dets_save_detail(Tab,Key,Detail),
        Det;
        Value -> Value
    end,
    dets_close(Tab),
    {ok, Val}.

dets_close(Tab) ->
    dets:close(Tab).

  
dets_fw_all() ->
    TAB = dets_open_fw(),
    dets:foldl(fun(X, L) -> [X|L] end, [],TAB).  

do_dets(Key,AddNodes) ->
    TAB = dets_open_fw(),
    dets:insert_new(TAB, {Key,AddNodes}),
    dets:close(TAB).

do_dets(Recs) ->
    TAB = dets_open_fw(),
    dets:insert(TAB,Recs),
    dets:close(TAB).

%[{node,[{<<"N0e1">>,"v4-6ddde99"},{<<"N0e2">>,"v4-6ddde99"},{<<"N0e3">>,"v4-6ddde99"},{<<"N0e4">>,"v4-6ddde99"},{<<"N0e5">>,"v4-6ddde99"}]}]

dets_fetch_fw(Key) ->
    TAB = dets_open_fw(),
    Recs = dets:lookup(TAB, Key),
    dets:close(TAB),
    Recs.

dets_replace_fw(Key, NewThings) ->
    TAB = dets_open_fw(),
    dets:insert(TAB,{Key,NewThings}), 
    dets:close(TAB).

dets_fetch_all_fw() ->
    TAB =dets_open_fw(),
    All = dets:foldl(fun(X, L) -> [X|L] end, [],TAB),
    dets:close(TAB),
    All.

dets_fw_delete(Key) ->
    TAB =dets_open_fw(),
    dets:delete(TAB,Key),
    dets:close(TAB).

dets_open_fw() ->
    {ok,[[Home]]} = init:get_argument(home),
    TAB = Home ++ "/" ++ "node_boots",
    dets:open_file(TAB,[{type,bag}]),
    TAB.