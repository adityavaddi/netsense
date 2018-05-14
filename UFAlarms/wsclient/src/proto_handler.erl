-module(proto_handler).
-author("Gina Hagg <ghagg@yahoo.com").
-compile(export_all).
-compile([{parse_transform, lager_transform}]).

-define(LastNumber,3).

decode_answer_and_get_next_question(factorial, Bin) ->
    Dcoded = math_calcul:decode_msg(Bin, factorial),
    lager:info("Dcoded: ~p ~n",[Dcoded]),
    handle_decoded(factorial,Dcoded);


decode_answer_and_get_next_question(_MsgName, Bin) ->
    lager:info("I am not interested in this message: ~p~n", [Bin]),
    %erlang:error(stop_sending_me_garbage),
    <<"stop_sending_me_garbage">>.

decode_incoming_payload(Msg) -> 
    lager:info("recvd binary: ~p~n", [Msg]),   
    {meta, MsgName, DecodedAnswer} = meta_proto:decode_msg(Msg,meta),
    lager:info("It is a ~p binary: ~p~n", [MsgName,DecodedAnswer]),
    {meta, MsgName, DecodedAnswer}.
   
%%for testing
decode_nested_message(factorial, Bin) ->
    Decoded = math_calcul:decode_msg(Bin, factorial),
    lager:info("Decoded: ~p ~n",[Decoded]),
    Decoded.

handle_decoded(factorial, Dcoded) when is_list(Dcoded) ->
    [handle_decoded(factorial,M) || M <- Dcoded];

handle_decoded(factorial,{factorial,Name, Number, Result}) ->
     %Prepare next factorial question. Add 2 to prev number.
     lager:info("server answer: factorial of ~p is ~p~n",[Number,Result]),
     NextNumber = Number + 1,
     lager:info("NextNumber: ~p",[NextNumber]),
     NextQ = math_calcul:encode_msg({factorial,Name,NextNumber, 0}),
     lager:info("NextQuestion: ~p~n",[NextQ]),
     NextQ.

encode_next_outgoing_question(MsgName, NextQuestion) ->
    meta_proto:encode_msg({meta,MsgName,NextQuestion}).

%%for testing
encodeFactorialMessage(Number) ->   
    Q = math_calcul:encode_msg({factorial,<<"factorial">>,Number, 0}),
    lager:info("Encoded: {factorial,<<\"factorial\">>,~p, 0} = ~p~n",[Number,Q]),
    M = meta_proto:encode_msg({meta,factorial,Q}),
    lager:info("Encoded with meta for final sendoff: ~p~n",[M]),
    M.

