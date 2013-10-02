#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/lib/*/ebin -boot start_sasl -s reloader -s engine -sname database -setcookie database
