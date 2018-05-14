%%%-------------------------------------------------------------------
%% @author Pierre Defebvre <pierre.defebvre@verizon.com>
%% @doc
%% == RabbitMQ Helpers ==
%% Series of helper functions for RabbitMQ
%% @end
%%%-------------------------------------------------------------------
-module(rabbit_helper).

-export([
    publish/3
    ,to_binary/1
]).

%%--------------------------------------------------------------------
%% @doc
%% Pull worker out of the pool and publish message
%% @end
%%--------------------------------------------------------------------
-spec publish(binary(), binary(), binary()) -> 'ok'.
publish(Exchange, Key, Payload) ->
    case elb:call('rabbit_load_balancer', {'publish', Exchange, Key, Payload}) of
        'ok' ->
            legacy_metrics:msg_rate_outgoing(1),
            lager:debug("worker responded with ok");
        Error ->
            lager:error(
                "worker responded with ~p publishing ~p Exch: ~p Key: ~p"
                ,[Error, Payload, Exchange, Key]
            )
    end.

%%--------------------------------------------------------------------
%% @doc
%% Take a variable and convert it to `binary()'
%% @end
%%--------------------------------------------------------------------
-spec to_binary(any()) -> binary().
to_binary(Atom) when is_atom(Atom)->
    erlang:atom_to_binary(Atom, 'utf8');
to_binary(Str) when is_list(Str)->
     binary:list_to_bin(Str);
to_binary(Number) when is_number(Number)->
    erlang:integer_to_binary(Number);
to_binary(Binary) when is_binary(Binary)->
    Binary.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
