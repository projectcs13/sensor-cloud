all: compile

#install:
#	@scripts/install.sh

compile:
	./rebar compile

clean: clean_emacs_vsn_files
	./rebar clean
	@rmdir ebin

clean_emacs_vsn_files:
	rm -rf *~
	rm -rf doc/*~
	rm -rf include/*~
	rm -rf priv/*~
	rm -rf scripts/*~
	rm -rf src/*~
	rm -rf test/*~

run:
	erl -pa ebin 

