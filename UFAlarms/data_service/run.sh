#!/bin/bash
trap "echo 'Completed' && exit" INT TERM
trap "kill 0" EXIT
lein with-profile main-proxy run &
lein with-profile main-worker run ${1:-} &
wait

