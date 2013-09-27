compile:
	./rebar compile
clean:
	./rebar clean
run:
	erl -pa ebin
deps:
	@./rebar get-deps
all: deps
	@./rebar compile

distclean: clean
	@./rebar delete-deps
