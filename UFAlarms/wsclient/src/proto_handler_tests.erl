-module(proto_handler_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    ok.
cleanup(_) ->
    ok.


%%this is the sequence wsclient follows.
wsclient_operations_test() ->
    Msg = <<8,9,18,15,10,9,102,97,99,116,111,114,105,97,108,16,2,24,2>>,
    DecodedAnswer = <<10,9,102,97,99,116,111,114,105,97,108,16,2,24,2>>,
    NextQ = <<10,9,102,97,99,116,111,114,105,97,108,16,3,24,0>>,
    EncodedNextQuestion = <<8,9,18,15,10,9,102,97,99,116,111,114,105,97,108,16,3,24,0>>,
    %DecodedNextQuestion = <<10,9,102,97,99,116,111,114,105,97,108,16,3,24,0>>,
    DecodedPayload = {meta, factorial, <<10,9,102,97,99,116,111,114,105,97,108,16,2,24,2>>},
    
    ?assertEqual(EncodedNextQuestion, wsclient:process_incoming_binary_and_prepare_next_question(Msg)),

    ?assertEqual(DecodedPayload,proto_handler:decode_incoming_payload(Msg)),
    
    ?assertEqual(NextQ , proto_handler:decode_answer_and_get_next_question(factorial,DecodedAnswer)),

    ?assertEqual(EncodedNextQuestion , proto_handler:encode_next_outgoing_question(factorial, NextQ)),

    ?assertEqual(EncodedNextQuestion , wsclient:encode_msg_for_sendoff(3)).


