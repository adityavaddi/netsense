-module(device_service_app).
-author("Gina Hagg ghagg@sensity.com").
-behaviour(application).

-export([start/2, start/0, stop/1]).

start() ->
    application:start(crypto),
	application:start(public_key),
	application:start(asn1),
	application:start(ssl),
	lager:start(),
	mnesia:start(),
	mnesia:change_table_copy_type(schema, node(), disc_copies),
	application:start(gen_listener_tcp),
	application:ensure_all_started(exometer),
	%rabbit_alarm:start(auto),
	device_service_sup:start_link().

start(_Type, _Args) ->
	device_service_sup:start_link().

stop(_State) ->
	ok.
