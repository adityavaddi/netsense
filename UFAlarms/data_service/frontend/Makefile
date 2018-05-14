
all: build

deps:
	lein npm update
	lein deps

clean:
	lein clean

build:
	mkdir -p out/frontend/resources
	cp resources/* out/frontend/resources
	lein cljsbuild once

DEBUG_FLAG=
debug: DEBUG_FLAG=--debug
debug: run

run: build
	cd out/frontend && node frontend.js $(DEBUG_FLAG)
