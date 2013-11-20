#!/usr/bin/env python
import datetime
import random
timestamp = datetime.datetime.now()
humidity = random.random()
print "Content-Type: application/json"
print
print """\
{"resource": "polling-resource", 
	"streams": 
		{ 
			"humidity": {"value": %f, "timestamp": "%s"}
		} 
	}
""" % (humidity, timestamp.strftime("%Y-%m-%d %H:%M:%S"))
