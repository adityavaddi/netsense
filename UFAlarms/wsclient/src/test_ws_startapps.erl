-module(test_ws_startapps).
-compile(export_all).
%List of applications
-define(APPLICATIONS, [asn1,gproc,fs,active,crypto,public_key,ssl,ranch,cowlib,gun,jsx,wsclient]).

start() ->
	lists:map(fun(X) -> ok = application:ensure_started(X) end, ?APPLICATIONS),
	io:format("~n~nAll applications started ~p~n~n",[application:which_applications()]).