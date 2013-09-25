all: compile
compile: compile_engine compile_website compile_deps
clean: clean_engine clean_website clean_deps clean_emacs_vsn_files

compile_deps:
	cd deps/nitrogen; make

compile_engine:
	cd engine; make

compile_website: compile_deps
	cd website; make

clean_emacs_vsn_files:
	rm -rf *~
	rm -rf doc/*~
	rm -rf include/*~
	rm -rf priv/*~
	rm -rf scripts/*~
	rm -rf src/*~
	rm -rf test/*~		

clean_engine:
	cd engine; make clean

clean_website:
	cd website; make clean

clean_deps:
	cd deps/nitrogen; make clean

#run: compile
#	erl

run_engine: compile_engine
	cd engine; make run

run_website: compile_website
	cd website; make run


