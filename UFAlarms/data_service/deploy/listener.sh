#!/bin/bash

mkdir -p /home/ubuntu/locks

function restart-service() {
  echo "$(date +"%D %T"): Processing data-service-deployed message..."
  docker login -e farallones-build@sensity.com -u teamfarallones -p teamfarallones2015 https://54.201.195.237:443
  docker pull 54.201.195.237:443/data-service
  docker stop data-service
  docker rm data-service
  docker run --name=data-service --restart=always -p 3000:3000 -t 54.201.195.237:443/data-service \
    sh -c "/root/run.sh" >> logs/data-service.log &
}

tail -n 0 -f logs/serf.log | grep --line-buffered "agent: Received event: user-event: data-service-deployed" | while read -r l; do
  flock -x /home/ubuntu/locks/listener.lock
  restart-service
done
