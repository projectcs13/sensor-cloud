################################################################################
###                       Makefile for Project CS 2013                       ###
################################################################################
### Variable assignment
################################################################################
ERL := erl
REBAR := ./rebar
ERL_CONFIG := -config config/engine.config
ERL_BOOT := -boot start_sasl -s reloader -s engine
ERL_PA_FOLDERS := -pa ebin/ lib/*/ebin/ lib/*/bin/
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
conf: compile_libs
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) -s engine -s config
	make compile_libs

get_libs:
	@@$(REBAR) get-deps

compile_libs:
	@$(REBAR) compile
	$(MAKE) -C lib/rabbitmq-server
	$(MAKE) -C lib/rabbitmq-erlang-client
	$(MAKE) -C lib/rErlang

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
all: compile

### Command: make install
### Downloads all dependencies and builds the entire project
install: get_libs conf
	echo "installing npm libs"
	(cd javascripts; npm install socket.io; npm install rabbit.js)
	 cp -r lib/elasticsearch-servicewrapper/service lib/elasticsearch/bin/


### Command: make run
### Downloads all depenedencies, bulds entire project and runs the project.
run: compile
	-export R_HOME="/usr/lib/R"
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine

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
	-@curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -s test run

test_travis: compile
	-@mkdir test-results
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -s test run

test_json: compile
	-@mkdir test-results
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(lib_json)'

test_resources: compile
	-@mkdir test-results
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(resources)'

test_streams: compile
	-@mkdir test-results
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(streams)'

## test_suggest: compile
	## -@mkdir test-results
	## curl -XDELETE localhost:9200/sensorcloud
	## curl -XPUT localhost:9200/sensorcloud
	## $(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(suggest)'

test_users: compile
	-@mkdir test-results
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(users)'

test_poll: compile
	-@mkdir test-results
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(poll_help)'

test_poll_system: compile
	-@mkdir test-results
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(polling_system)'

test_search: compile
	-@mkdir test-results
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(search)'

test_triggers: compile
	-@mkdir test-results
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(triggers)'


test_vstreams: compile
	-@mkdir test-results
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(virtual_stream_process)'


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
