#!/bin/sh

if [ -f "./datadealer-env.sh" ]; then
    . "./datadealer-env.sh"
fi

exec java -Djava.library.path=/usr/local/lib -Dname=docker $JVM_OPTS -jar dealer-1.0.0-SNAPSHOT-standalone.jar

exit $?
