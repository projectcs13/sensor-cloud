################################################################################
###                       Makefile for Project CS 2013                       ###
################################################################################
### Variable assignment
################################################################################
ERL ?= erl
APP := engine
REBAR=./rebar
REBAR_URL=http://cloud.github.com/downloads/basho/rebar/rebar
################################################################################


################################################################################
### Dependency rules
### Do NOT touch this section!
### The commands in this sections should not be used in general, but can be used
### if there is need for it
################################################################################
compile:
	@./rebar compile skip_deps=true

### get_libs will download and install all project libraries
get_libs:
	@./rebar get-deps
	@./rebar compile

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
install: get_libs

### Command: make run
### Downloads all depenedencies, bulds entire project and runs the project.
run: compile
	erl -pa ebin/ lib/*/ebin/ -boot start_sasl -s reloader -s engine -sname engine 

### Command: make run_es
### Runs elastic search
run_es:
	lib/elastic_search/bin/elasticsearch -f

### Command: make test
### Compile project resources (not libraries) and runs all eunit tests.
test: compile
	-@mkdir test-results
	erl -pa ebin/ lib/*/ebin/ -boot start_sasl -s reloader -s engine -sname test -s test run

### Command: make docs
### Genereats all of the documentation files
docs:
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