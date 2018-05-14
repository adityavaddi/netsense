-module(scraps).
-compile(export_all).


convert_to_binary_string(Sometuple) ->
	lists:map(fun(X) -> 
		case is_list(X) of 
			true -> iolist_to_binary(X);
			_ -> X
			end
			end, tuple_to_list(Sometuple)).