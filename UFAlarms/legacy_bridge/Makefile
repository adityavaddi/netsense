PROJECT = legacy_bridge

DEPS = cowboy lager gpb protobuffs jsx msgpack emqttc gproc erlcloud erlcron ezmq cqerl poolboy erlzmq2 recon observer_cli exometer
dep_cowboy = git https://github.com/extend/cowboy.git 1.0.2
dep_lager = git https://github.com/basho/lager.git 3.1.0
dep_gpb = git https://github.com/tomas-abrahamsson/gpb.git 3.20.1
dep_emqttc = git https://github.com/emqtt/emqttc.git master
dep_gproc = git https://github.com/uwiger/gproc.git master
dep_protobuffs = git https://github.com/basho/erlang_protobuffs.git
dep_jsx = git https://github.com/talentdeficit/jsx.git
dep_msgpack = git https://github.com/msgpack/msgpack-erlang.git 0.4.0
dep_erlcloud = git https://github.com/erlcloud/erlcloud.git
dep_erlcron = git https://github.com/Xeralux/erlcron.git
dep_ezmq = git https://github.com/Xeralux/ezmq.git
dep_cqerl = git https://github.com/matehat/cqerl.git v1.0.2
dep_poolboy = git https://github.com/devinus/poolboy.git 1.5.1
dep_erlzmq2 = git https://github.com/Xeralux/erlzmq2.git zmq3.2.5
dep_recon = git https://github.com/ferd/recon.git
dep_observer_cli = git https://github.com/zhongwencool/observer_cli.git
dep_exometer = git https://github.com/Feuerlabs/exometer.git
include erlang.mk

# Compile flags
ERLC_COMPILE_OPTS= +debug_info +'{parse_transform, lager_transform}'

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

GPROC_DIST=true
