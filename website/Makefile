all: compile

compile: compile_libs
	./rebar compile

compile_libs:
	-cd lib && tar xvfz nitrogen-yaws.tar.gz
	cd lib/nitrogen && make

clean: clean_emacs_vsn_files
	./rebar clean
	-rmdir ebin

clean_emacs_vsn_files:
	rm -rf *~
	rm -rf doc/*~
	rm -rf include/*~
	rm -rf priv/*~
	rm -rf scripts/*~
	rm -rf src/*~
	rm -rf test/*~

run:
	lib/nitrogen/bin/nitrogen console

