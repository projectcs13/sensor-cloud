################################################################################
###                       Makefile for Project CS 2013                       ###
################################################################################
################################################################################
################################################################################
### Variable assignment
################################################################################
ERL := erl
REBAR := ./rebar
ERL_CONFIG := -config config/engine.config -config config/sasl.config
ERL_BOOT := -boot start_sasl -s reloader -s engine
ERL_PA_FOLDERS := -pa ebin/ lib/*/ebin/ lib/*/bin/
TEST_RESULT_FOLDER := test-results
################################################################################
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
	(cd javascripts; npm install socket.io; npm install rabbit.js)

install_linux_deps:
	sudo scripts/install.sh

### Command: make run
### Downloads all depenedencies, bulds entire project and runs the project.
run: compile
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine

### Command: make run_all
### Starts all parts of the system in one command.
run_all: compile
	sudo scripts/sensec.sh start

### Command: make test_setup
### Runs all parts of the system except the erlang part. To be used prior to 'make test' for easy test setup
test_setup: compile
	sudo scripts/sensec.sh test_setup

### Command: make stop_all
### Stops all parts of the system in one command.
stop_all:
	sudo scripts/sensec.sh stop

### Command: make run_es
### Runs elastic search
run_es:
	lib/elasticsearch/bin/elasticsearch -f

### Command: make run_nodejs
### Runs NodeJS
run_nodejs:
	nodejs javascripts/receive.js

### Command: make run_fake_resource
### Runs the fake resources for polling	
run_fake_resource:
	(cd scripts/python/ && python -m CGIHTTPServer 8001 &) 
	(cd scripts/python/ && python -m CGIHTTPServer 8002)

### Command: make run_rabbit
### Runs rabbitMQ server
run_rabbit:
	sudo lib/rabbitmq-server/scripts/rabbitmq-server

### Command: make test
### Compile project resources (not libraries) and runs all eunit tests.
test: compile
	-@mkdir $(TEST_RESULT_FOLDER)
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -s test run

test_travis: compile
	-@mkdir $(TEST_RESULT_FOLDER)
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -s test run

test_datapoints: compile
	-@mkdir $(TEST_RESULT_FOLDER)
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(datapoints)'

test_json: compile
	-@mkdir $(TEST_RESULT_FOLDER)
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(lib_json)'

test_resources: compile
	-@mkdir $(TEST_RESULT_FOLDER)
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(resources)'

test_streams: compile
	-@mkdir $(TEST_RESULT_FOLDER)
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(streams)'

test_users: compile
	-@mkdir $(TEST_RESULT_FOLDER)
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(users)'

test_poll: compile
	-@mkdir $(TEST_RESULT_FOLDER)
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(poll_help)'

test_poll_system: compile
	-@mkdir $(TEST_RESULT_FOLDER)
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(polling_system)'

test_search: compile
	-@mkdir $(TEST_RESULT_FOLDER)
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(search)'

test_triggers: compile
	-@mkdir $(TEST_RESULT_FOLDER)
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(triggers)'


test_vstreams: compile
	-@mkdir $(TEST_RESULT_FOLDER)
	curl -XDELETE localhost:9200/sensorcloud
	curl -XPUT localhost:9200/sensorcloud
	$(ERL) $(ERL_PA_FOLDERS) $(ERL_CONFIG) $(ERL_BOOT) -sname engine -eval 'test:run(gen_virtual_stream_process)'


### Command: make docs
### Genereats all of the documentation files
docs: all
	./rebar doc skip_deps=true

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
	@echo "'make install_linux_deps'"
	@echo "Installs all linux dependencies needed. Should only be necessary to do once on a system."
	@echo ""
	@echo "'make run'"
	@echo "Compiles and runs the otp app. Does NOT compile libraries"
	@echo ""
	@echo "'make run_all'"
	@echo "Compiles the system (not libraries) and runs ALL parts of the system."
	@echo ""
	@echo "'make stop_all'"
	@echo "Stops all parts of the system started with 'make run_all'"
	@echo ""
	@echo "'make run_all'"
	@echo "Compiles and runs all parts of the project. Does NOT compile libraries"
	@echo ""
	@echo "'make test_setup'"
	@echo "Easy setup of environment prior to running 'make test''"
	@echo ""
	@echo "'make stop_all'"
	@echo "Stops all parts of the project which was started with 'make run_all'"
	@echo ""
	@echo "'make run_es'"
	@echo "Runs the elastic search server"
	@echo ""
	@echo "'make run_nodejs'"
	@echo "Runs the elastic nodejs"
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
