-module(library).
-compile(export_all).

int_ceil(X) ->
    T = trunc(X),
    if
        X > T -> T + 1;
        true  -> T
    end.

int_floor(X) ->
    T = trunc(X),
    if
        X < T -> T - 1;
        true  -> T
    end.


n_length_chunks_fast(List,Len) ->
    SkipLength = case length(List) rem Len of
        0 -> 0;
        N -> Len - N
    end,
    n_length_chunks_fast(lists:reverse(List),[],SkipLength,Len).

n_length_chunks_fast([],Acc,_Pos,_Max) -> Acc;

n_length_chunks_fast([H|T],Acc,Pos,Max) when Pos==Max ->
    n_length_chunks_fast(T,[[H] | Acc],1,Max);

n_length_chunks_fast([H|T],[HAcc | TAcc],Pos,Max) ->
    n_length_chunks_fast(T,[[H | HAcc] | TAcc],Pos+1,Max);

n_length_chunks_fast([H|T],[],Pos,Max) ->
    n_length_chunks_fast(T,[[H]],Pos+1,Max).


listOrd1(Searched, List) when is_list(List) ->
    listOrd1(Searched, List, 1).

listOrd1(_, [], _) -> nil;

listOrd1(Searched, [ Searched | _ ], Count) ->
    Count;

listOrd1(Searched, [ _ | Tail ], Count) ->

listOrd1(Searched, Tail, Count + 1).

now_us({MegaSecs,Secs,MicroSecs}) ->
  (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

%% Work from the back and keep a tail of things done.
flatten3(L) -> flatten3(L, []).

flatten3([H|T], Tail) ->
    flatten3(H, flatten3(T, Tail));
flatten3([], Tail) -> Tail;
flatten3(E, Tail) -> [E|Tail].

%% Lift nested lists to the front of the list.
flatten1([[H|T1]|T2]) -> flatten1([H,T1|T2]);
flatten1([[]|T]) -> flatten1(T);
flatten1([E|T]) -> [E|flatten1(T)];
flatten1([]) -> [].

%% Keep a list of things todo and put tails onto it.
flatten2(L) -> flatten2(L, []).

flatten2([H|T], Todo) ->
    flatten2(H, [T|Todo]);
flatten2([], [H|Todo]) -> flatten2(H, Todo);
flatten2([], []) -> [];
flatten2(E, Todo) -> [E|flatten2(Todo, [])].

%% setnth(Index, List, NewElement) -> List.

setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].