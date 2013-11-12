#!/bin/bash

#Check if user has root access
if [ "$USER" = "root" ]; then
	if [ "$1" = "start" ]; then
		#Create log directory if it does not already exist
		now="$(date +%d_%m_%Y_%H_%M_%S)"		
		mkdir -p logs/"$now"
		chmod 777 logs
		echo "Starting up RabbitMQ-Server"
		./lib/rabbitmq-server/scripts/rabbitmq-server > logs/"$now"/rabbit_log.log &
		echo "Starting up ElasticSearch"
		./lib/elasticsearch/bin/service/elasticsearch console > logs/"$now"/es_log.log &
		if [ -s .temp.log ]; then
   		   echo "Sensor cloud already running, to close run ./sensec.sh stop"
		else
		   sleep 2
		   echo "Starting node.js with receive.js"
		   nodejs javascripts/receive.js >logs/"$now"/nodejs_log.log &
		   echo $! > .temp.log
		   echo "Starting Sensor-Cloud"
      		   export R_HOME="/usr/lib/R"
		   erl -noshell -pa ebin/ lib/*/ebin/ lib/*/bin/ -boot start_sasl -s reloader -s engine -sname engine > logs/"$now"/sensor-cloud_log.log &
		   echo $! >> .temp.log
		fi
	 elif [ "$1" = "stop" ]; then
		echo "Closing nodejs and Sensor-Cloud"
		while read line
		do
			kill -15 "$line"
		done < .temp.log
		rm .temp.log

		echo "Closing down RabbitMQ-Server"
		./lib/rabbitmq-server/scripts/rabbitmqctl stop
		echo "Closing down ElasticSearch"
		./lib/elasticsearch/bin/service/elasticsearch stop
	fi
else 
	echo "$USER has no root access, use sudo"
fi


