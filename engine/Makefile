all: deps compile

compile: compile_libs compile_src

compile_src:
	./rebar compile

compile_libs:



clean: clean_emacs_vsn_files
	./rebar clean
	rm -f erl_crash.dump
	rm -rf ebin/

clean_docs:
	rm -f doc/*.html
	rm -f doc/edoc-info
	rm -f doc/erlang.png
	rm -f doc/stylesheet.css

clean_emacs_vsn_files:
	rm -rf *~
	rm -rf doc/*~
	rm -rf include/*~
	rm -rf priv/*~
	rm -rf scripts/*~
	rm -rf src/*~
	rm -rf test/*~

run: compile
	erl -pa ebin -sname database -setcookie database -mnesia dir '"/home/database/Mnesia.Database"' -s database init

run_restful:
	./start.sh

docs: compile_src
	erl -pa ebin -s start doc

deps:
	@./rebar get-deps

distclean: clean
	@./rebar delete-deps

