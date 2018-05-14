.PHONY: all typecheck compile frontend run test clean

all: compile frontend

compile:
	lein compile

frontend:
	+cd frontend && make

STUBDATA=
debug: STUBDATA=--stubdata
debug: run

run:
	./run.sh $(STUBDATA)

test:
	lein test

clean:
	lein clean
