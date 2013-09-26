all: compile

#install:
#	@scripts/install.sh

compile:
	./rebar compile

clean: clean_emacs_vsn_files
	./rebar clean

clean_emacs_vsn_files:
	rm -rf *~
	rm -rf doc/*~
	rm -rf include/*~
	rm -rf priv/*~
	rm -rf scripts/*~
	rm -rf src/*~
	rm -rf test/*~

run:
	erl -pa ebin -sname database -setcookie database -mnesia dir '"/home/database/Mnesia.Database"' -s database init
