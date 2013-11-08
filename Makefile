################################################################################
###                       Makefile for Project CS 2013                       ###
################################################################################
### Variable assignment
################################################################################
ERL := erl
REBAR := ./rebar
# DIALYZE_INCLUDE = -I lib/erlastic_search/include/ -I lib/webmachine/include/ -I lib/rabbitmq-erlang-client/include -I lib/erlson/include/
################################################################################


################################################################################
### Dependency rules
### Do NOT touch this section!
### The commands in this sections should not be used in general, but can be used
### if there is need for it
################################################################################
compile:
	@$(REBAR) compile skip_deps=true

### get_libs will download and install all project libraries
get_libs:
	@@$(REBAR) get-deps
	@$(REBAR) compile
	$(MAKE) -C lib/rabbitmq-server
	$(MAKE) -C lib/rabbitmq-erlang-client
	$(MAKE) -C lib/rErlang

# prep_dialyzer:
# 	dialyzer --build_plt --apps kernel stdlib erts mnesia eunit

# dialyze: 
# 	dialyzer -pa ebin/ $(DIALYZE_INCLUDE) src/*.erl lib/erlson/ebin/

clean_emacs_vsn_files:
	rm -rf *~
	rm -rf doc/*~
	rm -rf include/*~
	rm -rf priv/*~
	rm -rf scripts/*~
	rm -rf src/*~
	rm -rf test/*~
################################################################################


################################################################################
### Command rules
### This section contains commands that can be used.
### This section can be edited if needed
################################################################################

### Command: make
### Builds the entire project, excluding the dependencies.
all: compile #dialyze

### Command: make install
### Downloads all dependencies and builds the entire project
install: get_libs #prep_dialyzer

### Command: make run
### Downloads all depenedencies, bulds entire project and runs the project.
run: compile
	-export R_HOME="/usr/lib/R"
	$(ERL) -pa ebin/ lib/*/ebin/ lib/*/bin/ -boot start_sasl -s reloader -s engine -sname engine 

### Command: make run_es
### Runs elastic search
run_es:
	lib/elasticsearch/bin/elasticsearch -f

### Command: make run_rabbit
### Runs rabbitMQ server
run_rabbit:
	sudo lib/rabbitmq-server/scripts/rabbitmq-server

### Command: make test
### Compile project resources (not libraries) and runs all eunit tests.
test: compile
	-@mkdir test-results
	$(ERL) -pa ebin/ lib/*/ebin/ lib/*/bin/ -boot start_sasl -s reloader -s engine -sname engine -s test run

test_json: compile
	-@mkdir test-results
	$(ERL) -pa ebin/ lib/*/ebin/ -boot start_sasl -s reloader -s engine -sname engine -eval 'test:run(lib_json)'

test_resource: compile
	-@mkdir test-results
	$(ERL) -pa ebin/ lib/*/ebin/ -boot start_sasl -s reloader -s engine -sname engine -eval 'test:run(resource)'

test_streams: compile
	-@mkdir test-results
	$(ERL) -pa ebin/ lib/*/ebin/ -boot start_sasl -s reloader -s engine -sname engine -eval 'test:run(streams)'

test_suggest: compile
	-@mkdir test-results
	$(ERL) -pa ebin/ lib/*/ebin/ -boot start_sasl -s reloader -s engine -sname engine -eval 'test:run(suggest)'

test_users: compile
	-@mkdir test-results
	$(ERL) -pa ebin/ lib/*/ebin/ -boot start_sasl -s reloader -s engine -sname engine -eval 'test:run(users)'

### Command: make docs
### Genereats all of the documentation files
docs: all
	./rebar skip_deps=true doc

### Command: make clean
### Cleans the directory of the following things:
### * Emacs versioning files.
### * All erlang .beam files, including 'ebin' folder
clean: clean_emacs_vsn_files
	@./rebar clean skip_deps=true
	rm -f erl_crash.dump
	rm -rf ebin/
	rm -rf test-results/

### Command: make clean_libs
### Cleans the directory of the following things:
### * All the downloaded libraries
clean_libs:
	@./rebar delete-deps
	rm -rf lib/

### Command: make clean_docs
### Cleans the directory of the following things:
### * All the documentation files except 'overview.edoc'
clean_docs:
	find doc/ -type f -not -name 'overview.edoc' | xargs rm

### Command: make help
### Prints an explanation of the commands in this Makefile
help:
	@echo "###################################################################"
	@echo "Commands:"
	@echo ""
	@echo "'make'"
	@echo "Compiles all the project sources. Does NOT compile libraries"
	@echo ""
	@echo "'make install'"
	@echo "Downloads and compiles all libraries"
	@echo ""
	@echo "'make run'"
	@echo "Compiles and runs the project. Does NOT compile libraries"
	@echo ""
	@echo "'make run_es'"
	@echo "Runs the elastic search server"
	@echo ""
	@echo "make run_rabbit"
	@echo "Runs the rabbitMQ server"
	@echo ""
	@echo "'make docs'"
	@echo "Generates documentation for the project"
	@echo ""
	@echo "'make clean'"
	@echo "Cleans all the project, including dependencies"
	@echo ""
	@echo "'make clean_libs'"
	@echo "Cleans all of the libraries"
	@echo ""
	@echo "'make clean_docs'"
	@echo "Cleans all of the documentation files, except for 'overview.edoc'"
	@echo ""
	@echo "'make help'"
	@echo "Prints an explanation of the commands in this Makefile"
	@echo "###################################################################"
