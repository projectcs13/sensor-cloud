import os
import time
import sys
import urllib
import urllib2

f = open('realstreams', 'r')
url = "http://localhost:8000/streams" 

for line in f:
	req = urllib2.Request(url)
	print line
        req.add_data(line)
	response = urllib2.urlopen(req)
	print response.read()
	time.sleep(0.1)
