#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/lib/*/ebin -boot start_sasl -s reloader -s engine -sname database -setcookie database -mnesia dir '"/home/database/Mnesia.Database"' -s database init
