#!/usr/bin/env python
import datetime
import random
timestamp = datetime.datetime.now()
humidity = random.random()
temperature = random.random() * 100
print "Content-Type: application/json"
print
print """\
{"resource": "polling-resource", 
	"streams": 
		{ 
			"temperature": {"value": %f, "timestamp": "%s"}, 
			"humidity": {"value": %f, "timestamp": "%s"}
		} 
	}
""" % (temperature, timestamp.strftime("%Y-%m-%d %H:%M:%S"), humidity, timestamp.strftime("%Y-%m-%d %H:%M:%S"))
