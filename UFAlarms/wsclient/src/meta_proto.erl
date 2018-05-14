%% Automatically generated, do not edit
%% Generated by gpb_compile version 3.20.1 on {{2016,5,30},{15,53,21}}
-module(meta_proto).

-export([encode_msg/1, encode_msg/2]).
-export([decode_msg/2]).
-export([merge_msgs/2]).
-export([verify_msg/1]).
-export([get_msg_defs/0]).
-export([get_msg_names/0]).
-export([get_enum_names/0]).
-export([find_msg_def/1, fetch_msg_def/1]).
-export([find_enum_def/1, fetch_enum_def/1]).
-export([enum_symbol_by_value/2, enum_value_by_symbol/2]).
-export(['enum_symbol_by_value_meta.msgtype'/1, 'enum_value_by_symbol_meta.msgtype'/1]).
-export([get_service_names/0]).
-export([get_service_def/1]).
-export([get_rpc_names/1]).
-export([find_rpc_def/2, fetch_rpc_def/2]).
-export([get_package_name/0]).
-export([gpb_version_as_string/0, gpb_version_as_list/0]).

-include("meta_proto.hrl").
-include("gpb.hrl").


encode_msg(Msg) -> encode_msg(Msg, []).


encode_msg(Msg, Opts) ->
    case proplists:get_bool(verify, Opts) of
      true -> verify_msg(Msg);
      false -> ok
    end,
    case Msg of #meta{} -> e_msg_meta(Msg) end.


'e_enum_meta.msgtype'(meta, Bin) -> <<Bin/binary, 0>>;
'e_enum_meta.msgtype'(ping, Bin) -> <<Bin/binary, 1>>;
'e_enum_meta.msgtype'(pong, Bin) -> <<Bin/binary, 2>>;
'e_enum_meta.msgtype'(person, Bin) -> <<Bin/binary, 3>>;
'e_enum_meta.msgtype'(location, Bin) ->
    <<Bin/binary, 4>>;
'e_enum_meta.msgtype'(short, Bin) -> <<Bin/binary, 5>>;
'e_enum_meta.msgtype'(areaofcircle, Bin) ->
    <<Bin/binary, 6>>;
'e_enum_meta.msgtype'(areaofsquare, Bin) ->
    <<Bin/binary, 7>>;
'e_enum_meta.msgtype'(areaofrectangle, Bin) ->
    <<Bin/binary, 8>>;
'e_enum_meta.msgtype'(factorial, Bin) ->
    <<Bin/binary, 9>>.

e_msg_meta(Msg) -> e_msg_meta(Msg, <<>>).


e_msg_meta(#meta{type = F1, msg = F2}, Bin) ->
    B1 = 'e_enum_meta.msgtype'(F1, <<Bin/binary, 8>>),
    e_type_bytes(F2, <<B1/binary, 18>>).

e_type_bytes(Bytes, Bin) ->
    Bin2 = e_varint(byte_size(Bytes), Bin),
    <<Bin2/binary, Bytes/binary>>.

e_varint(N, Bin) when N =< 127 -> <<Bin/binary, N>>;
e_varint(N, Bin) ->
    Bin2 = <<Bin/binary, (N band 127 bor 128)>>,
    e_varint(N bsr 7, Bin2).



decode_msg(Bin, MsgName) when is_binary(Bin) ->
    case MsgName of meta -> d_msg_meta(Bin) end.



'd_enum_meta.msgtype'(0) -> meta;
'd_enum_meta.msgtype'(1) -> ping;
'd_enum_meta.msgtype'(2) -> pong;
'd_enum_meta.msgtype'(3) -> person;
'd_enum_meta.msgtype'(4) -> location;
'd_enum_meta.msgtype'(5) -> short;
'd_enum_meta.msgtype'(6) -> areaofcircle;
'd_enum_meta.msgtype'(7) -> areaofsquare;
'd_enum_meta.msgtype'(8) -> areaofrectangle;
'd_enum_meta.msgtype'(9) -> factorial.

d_msg_meta(Bin) ->
    dfp_read_field_def_meta(Bin, 0, 0, undefined,
			    undefined).

dfp_read_field_def_meta(<<8, Rest/binary>>, Z1, Z2, F1,
			F2) ->
    d_field_meta_type(Rest, Z1, Z2, F1, F2);
dfp_read_field_def_meta(<<18, Rest/binary>>, Z1, Z2, F1,
			F2) ->
    d_field_meta_msg(Rest, Z1, Z2, F1, F2);
dfp_read_field_def_meta(<<>>, 0, 0, F1, F2) ->
    #meta{type = F1, msg = F2};
dfp_read_field_def_meta(Other, Z1, Z2, F1, F2) ->
    dg_read_field_def_meta(Other, Z1, Z2, F1, F2).

dg_read_field_def_meta(<<1:1, X:7, Rest/binary>>, N,
		       Acc, F1, F2)
    when N < 32 - 7 ->
    dg_read_field_def_meta(Rest, N + 7, X bsl N + Acc, F1,
			   F2);
dg_read_field_def_meta(<<0:1, X:7, Rest/binary>>, N,
		       Acc, F1, F2) ->
    Key = X bsl N + Acc,
    case Key of
      8 -> d_field_meta_type(Rest, 0, 0, F1, F2);
      18 -> d_field_meta_msg(Rest, 0, 0, F1, F2);
      _ ->
	  case Key band 7 of
	    0 -> skip_varint_meta(Rest, 0, 0, F1, F2);
	    1 -> skip_64_meta(Rest, 0, 0, F1, F2);
	    2 -> skip_length_delimited_meta(Rest, 0, 0, F1, F2);
	    5 -> skip_32_meta(Rest, 0, 0, F1, F2)
	  end
    end;
dg_read_field_def_meta(<<>>, 0, 0, F1, F2) ->
    #meta{type = F1, msg = F2}.

d_field_meta_type(<<1:1, X:7, Rest/binary>>, N, Acc, F1,
		  F2)
    when N < 57 ->
    d_field_meta_type(Rest, N + 7, X bsl N + Acc, F1, F2);
d_field_meta_type(<<0:1, X:7, Rest/binary>>, N, Acc, _,
		  F2) ->
    <<Tmp:32/signed-native>> = <<(X bsl N +
				    Acc):32/unsigned-native>>,
    NewFValue = 'd_enum_meta.msgtype'(Tmp),
    dfp_read_field_def_meta(Rest, 0, 0, NewFValue, F2).


d_field_meta_msg(<<1:1, X:7, Rest/binary>>, N, Acc, F1,
		 F2)
    when N < 57 ->
    d_field_meta_msg(Rest, N + 7, X bsl N + Acc, F1, F2);
d_field_meta_msg(<<0:1, X:7, Rest/binary>>, N, Acc, F1,
		 _) ->
    Len = X bsl N + Acc,
    <<Bytes:Len/binary, Rest2/binary>> = Rest,
    NewFValue = binary:copy(Bytes),
    dfp_read_field_def_meta(Rest2, 0, 0, F1, NewFValue).


skip_varint_meta(<<1:1, _:7, Rest/binary>>, Z1, Z2, F1,
		 F2) ->
    skip_varint_meta(Rest, Z1, Z2, F1, F2);
skip_varint_meta(<<0:1, _:7, Rest/binary>>, Z1, Z2, F1,
		 F2) ->
    dfp_read_field_def_meta(Rest, Z1, Z2, F1, F2).


skip_length_delimited_meta(<<1:1, X:7, Rest/binary>>, N,
			   Acc, F1, F2)
    when N < 57 ->
    skip_length_delimited_meta(Rest, N + 7, X bsl N + Acc,
			       F1, F2);
skip_length_delimited_meta(<<0:1, X:7, Rest/binary>>, N,
			   Acc, F1, F2) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    dfp_read_field_def_meta(Rest2, 0, 0, F1, F2).


skip_32_meta(<<_:32, Rest/binary>>, Z1, Z2, F1, F2) ->
    dfp_read_field_def_meta(Rest, Z1, Z2, F1, F2).


skip_64_meta(<<_:64, Rest/binary>>, Z1, Z2, F1, F2) ->
    dfp_read_field_def_meta(Rest, Z1, Z2, F1, F2).




merge_msgs(Prev, New)
    when element(1, Prev) =:= element(1, New) ->
    case Prev of #meta{} -> merge_msg_meta(Prev, New) end.

merge_msg_meta(#meta{type = PFtype, msg = PFmsg},
	       #meta{type = NFtype, msg = NFmsg}) ->
    #meta{type =
	      if NFtype =:= undefined -> PFtype;
		 true -> NFtype
	      end,
	  msg =
	      if NFmsg =:= undefined -> PFmsg;
		 true -> NFmsg
	      end}.



verify_msg(Msg) ->
    case Msg of
      #meta{} -> v_msg_meta(Msg, [meta]);
      _ -> mk_type_error(not_a_known_message, Msg, [])
    end.


v_msg_meta(#meta{type = F1, msg = F2}, Path) ->
    'v_enum_meta.msgtype'(F1, [type | Path]),
    v_type_bytes(F2, [msg | Path]),
    ok.

'v_enum_meta.msgtype'(meta, _Path) -> ok;
'v_enum_meta.msgtype'(ping, _Path) -> ok;
'v_enum_meta.msgtype'(pong, _Path) -> ok;
'v_enum_meta.msgtype'(person, _Path) -> ok;
'v_enum_meta.msgtype'(location, _Path) -> ok;
'v_enum_meta.msgtype'(short, _Path) -> ok;
'v_enum_meta.msgtype'(areaofcircle, _Path) -> ok;
'v_enum_meta.msgtype'(areaofsquare, _Path) -> ok;
'v_enum_meta.msgtype'(areaofrectangle, _Path) -> ok;
'v_enum_meta.msgtype'(factorial, _Path) -> ok;
'v_enum_meta.msgtype'(X, Path) ->
    mk_type_error({invalid_enum, 'meta.msgtype'}, X, Path).

v_type_bytes(B, _Path) when is_binary(B) -> ok;
v_type_bytes(X, Path) ->
    mk_type_error(bad_binary_value, X, Path).

mk_type_error(Error, ValueSeen, Path) ->
    Path2 = prettify_path(Path),
    erlang:error({gpb_type_error,
		  {Error, [{value, ValueSeen}, {path, Path2}]}}).


prettify_path([]) -> top_level;
prettify_path(PathR) ->
    list_to_atom(string:join(lists:map(fun atom_to_list/1,
				       lists:reverse(PathR)),
			     ".")).



get_msg_defs() ->
    [{{enum, 'meta.msgtype'},
      [{meta, 0}, {ping, 1}, {pong, 2}, {person, 3},
       {location, 4}, {short, 5}, {areaofcircle, 6},
       {areaofsquare, 7}, {areaofrectangle, 8},
       {factorial, 9}]},
     {{msg, meta},
      [#field{name = type, fnum = 1, rnum = 2,
	      type = {enum, 'meta.msgtype'}, occurrence = required,
	      opts = []},
       #field{name = msg, fnum = 2, rnum = 3, type = bytes,
	      occurrence = required, opts = []}]}].


get_msg_names() -> [meta].


get_enum_names() -> ['meta.msgtype'].


fetch_msg_def(MsgName) ->
    case find_msg_def(MsgName) of
      Fs when is_list(Fs) -> Fs;
      error -> erlang:error({no_such_msg, MsgName})
    end.


fetch_enum_def(EnumName) ->
    case find_enum_def(EnumName) of
      Es when is_list(Es) -> Es;
      error -> erlang:error({no_such_enum, EnumName})
    end.


find_msg_def(meta) ->
    [#field{name = type, fnum = 1, rnum = 2,
	    type = {enum, 'meta.msgtype'}, occurrence = required,
	    opts = []},
     #field{name = msg, fnum = 2, rnum = 3, type = bytes,
	    occurrence = required, opts = []}];
find_msg_def(_) -> error.


find_enum_def('meta.msgtype') ->
    [{meta, 0}, {ping, 1}, {pong, 2}, {person, 3},
     {location, 4}, {short, 5}, {areaofcircle, 6},
     {areaofsquare, 7}, {areaofrectangle, 8},
     {factorial, 9}];
find_enum_def(_) -> error.


enum_symbol_by_value('meta.msgtype', Value) ->
    'enum_symbol_by_value_meta.msgtype'(Value).


enum_value_by_symbol('meta.msgtype', Sym) ->
    'enum_value_by_symbol_meta.msgtype'(Sym).


'enum_symbol_by_value_meta.msgtype'(0) -> meta;
'enum_symbol_by_value_meta.msgtype'(1) -> ping;
'enum_symbol_by_value_meta.msgtype'(2) -> pong;
'enum_symbol_by_value_meta.msgtype'(3) -> person;
'enum_symbol_by_value_meta.msgtype'(4) -> location;
'enum_symbol_by_value_meta.msgtype'(5) -> short;
'enum_symbol_by_value_meta.msgtype'(6) -> areaofcircle;
'enum_symbol_by_value_meta.msgtype'(7) -> areaofsquare;
'enum_symbol_by_value_meta.msgtype'(8) ->
    areaofrectangle;
'enum_symbol_by_value_meta.msgtype'(9) -> factorial.


'enum_value_by_symbol_meta.msgtype'(meta) -> 0;
'enum_value_by_symbol_meta.msgtype'(ping) -> 1;
'enum_value_by_symbol_meta.msgtype'(pong) -> 2;
'enum_value_by_symbol_meta.msgtype'(person) -> 3;
'enum_value_by_symbol_meta.msgtype'(location) -> 4;
'enum_value_by_symbol_meta.msgtype'(short) -> 5;
'enum_value_by_symbol_meta.msgtype'(areaofcircle) -> 6;
'enum_value_by_symbol_meta.msgtype'(areaofsquare) -> 7;
'enum_value_by_symbol_meta.msgtype'(areaofrectangle) ->
    8;
'enum_value_by_symbol_meta.msgtype'(factorial) -> 9.


get_service_names() -> [].


get_service_def(_) -> error.


get_rpc_names(_) -> error.


find_rpc_def(_, _) -> error.



fetch_rpc_def(ServiceName, RpcName) ->
    erlang:error({no_such_rpc, ServiceName, RpcName}).


get_package_name() -> 'erproto.meta'.



gpb_version_as_string() ->
    "3.20.1".

gpb_version_as_list() ->
    [3,20,1].
