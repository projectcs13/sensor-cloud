ERL ?= erl
APP := website

all: compile

compile: get_libs compile_src

compile_src: get_libs
	./rebar compile

get_libs:
	@./rebar get-deps


clean: clean_libs clean_emacs_vsn_files
	./rebar clean
	rm -f erl_crash.dump
	rm -rf ebin/

clean_docs:
	find doc/ -type f -not -name 'overview.edoc' | xargs rm

clean_libs:
	@./rebar delete-deps
	rm -rf lib/

clean_emacs_vsn_files:
	rm -rf *~
	rm -rf doc/*~
	rm -rf include/*~
	rm -rf priv/*~
	rm -rf scripts/*~
	rm -rf src/*~
	rm -rf test/*~

run: compile
	./start.sh

docs: compile_src
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{packages, false},{private, true}]'




