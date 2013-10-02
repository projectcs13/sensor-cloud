################################################################################
###                       Makefile for Project CS 2013                       ###
################################################################################
### Variable assignment
################################################################################
ERL ?= erl
APP := website
################################################################################


################################################################################
### Dependency rules
### Do NOT touch this section!
### The commands in this sections should not be used in general, but can be used
### if there is need for it
################################################################################
compile: get_libs compile_src

compile_src: get_libs
	./rebar compile

get_libs:
	@./rebar get-deps

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
################################################################################


################################################################################
### Command rules
### This section contains commands that can be used.
### This section can be edited if needed
################################################################################

### Command: make
### Downloads all dependencies and builds the entire project
all: compile

### Command: make clean
### Cleans the directory of the following things:
### * Libraries, including 'lib' folder.
### * Emacs versioning files.
### * All erlang .beam files, including 'ebin' folder
clean: clean_libs clean_emacs_vsn_files
	./rebar clean
	rm -f erl_crash.dump
	rm -rf ebin/

### Command: make clean_docs
### Cleans the directory of the following things:
### * All the documentation files except 'overview.edoc'
clean_docs:
	find doc/ -type f -not -name 'overview.edoc' | xargs rm

### Command: make run
### Downloads all depenedencies, bulds entire project and runs the project.
run: compile
	./start.sh

### Command: make docs
### Genereats all of the documentation files
docs: compile_src
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{packages, false},{private, true}]'