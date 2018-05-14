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

    