#!/bin/bash
/usr/local/bin/serf agent -iface eth0 >> /home/ubuntu/logs/serf.log 2>&1 &

function join() {
  until echo "$(date +"%D %T"): Attempting to join 11.0.235.245..." && /usr/local/bin/serf join 11.0.235.245; do sleep 1; done
}

join # join with interval of 1 second until healthy
while true; do join; sleep 10; done # while healthy, re-join every 10 seconds

