#!/bin/bash

trap "echo 'Completed' && exit" INT TERM
trap "kill 0" EXIT

cd /src/Farallones && git pull

backend() {
  cd /src/Farallones/data_service
  make debug
}

frontend() {
  cd /src/Farallones/data_service/frontend
  make deps && make run
}

export -f backend frontend
parallel --tag --line-buffer ::: backend frontend
