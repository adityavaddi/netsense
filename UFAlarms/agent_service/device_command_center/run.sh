#!/bin/sh
# -*- tab-width:4;indent-tabs-mode:nil -*-
# ex: ts=4 sw=4 et

#erl -pa ebin -pa deps/*/ebin -smp true -s device_service_app 
#erl -pa ebin -pa deps/*/ebin -smp true -eval "device_service_app:start(ok,ok)"
erl -pa ebin -pa deps/*/ebin -smp true -eval "device_service_app:start()" -config rel/sys.config -setcookie farallones
