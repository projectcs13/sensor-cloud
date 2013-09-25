compile:
	./rebar compile
clean:
	./rebar clean
run:
	erl -pa ebin

setup_database:
	erl -pz 'ebin/' -sname database -setcookie database -mnesia dir '"/home/database/Mnesia.Database"' -s database init
