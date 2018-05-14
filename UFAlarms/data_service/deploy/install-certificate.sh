#!/bin/bash

cp /home/ubuntu/Farallones/data_service/deploy/farallones-docker-registry.crt /usr/local/share/ca-certificates
update-ca-certificates
service docker restart

