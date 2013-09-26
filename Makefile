all: compile
compile: compile_engine compile_website
clean: clean_engine clean_website clean_deps clean_emacs_vsn_files

compile_engine:
	cd engine; make

compile_website:
	cd website; make

clean_emacs_vsn_files:
	rm -rf *~

clean_engine:
	cd engine/ && make clean

clean_website:
	cd website/ && make clean

clean_deps:
	-cd deps/nitrogen/ && make clean

#run: compile
#	erl

run_engine: compile_engine
	cd engine; make run

run_website: compile_website
	cd website; make run


