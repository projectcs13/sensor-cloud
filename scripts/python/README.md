## Resource for polling

This script acts as a dummy resource which you can poll.
You start a small webserver which you can request a 
resource JSON-object from.

To use the script, you start a CGI HTTP server in python.

From the terminal, in this directory, type:

    python -m CGIHTTPServer 8000

The port number (8000) can be whatever.
You will now have the resource accessable at
loclahost:8000/cgi-bin/resource.py

If you get a restriction-error upon GET-request, 
Make sure you have the right permissions to resource.py
by typing

    chmod 755 resource.py

## Self-posting stream

The script post_avg.py reads the average load from the file "/proc/loadavg" and sends the value to the specified
stream at the engine url. the script is called using:

    python post_avg.py <stream_id> <interval_in_seconds> <engine_base_address>

Example use

    python post_avg.py asd21 30 http://localhost:8000

the above example will post '{"value": <load_avg>}' every 30 seconds to http://localhost:8000/streams/asd21/data

The script posts using curl and thus requires curl to be installed.

## Post streams to localhost:8000

The script poststreams.py will post streams in a file to localhost:8000/streams, the example files are 
weatherstreams, financialstreams and gamestreams. These files contains the JSON objects of a number of streams
and they have the user_id 'andreas'. Either make sure that the user 'andreas' exists or change the user_ids in
the files.

If you want to make your own streamfiles, make sure that each stream object is contained in ONE line and that
there are no empty lines between objects. Also make sure that the file does not contain trailing endlines or
whitespaces.

Example usages:
	python poststreams.py weatherstreams

	python poststreams.py financialstreams

	python poststreams.py gamestreams


