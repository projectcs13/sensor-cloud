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
