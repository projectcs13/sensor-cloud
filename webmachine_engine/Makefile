ERL ?= erl
APP := webmachine_engine

.PHONY: deps

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

api-tests:
    $(shell python -m unittest discover test '*_tests.py' -v)
