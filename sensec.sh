#!/bin/bash
if [ "$USER" = "root" ]; then
	if [ "$1" = "start" ]; then
		#Create log directory if it does not already exist
		now="$(date +%d_%m_%Y_%H_%M_%S)"		
		mkdir -p logs/"$now"
		chmod 777 logs
		echo "Starting up RabbitMQ-Server"
		make run_rabbit > logs/"$now"/rabbit_log.log &
		echo "Starting up ElasticSearch"
		./lib/elasticsearch/bin/service/elasticsearch console > logs/"$now"/es_log.log &
		echo "Starting Sensor-Cloud"
		if [ -s .temp.log ]; then
   		   echo "Sensor cloud already running, to close run ./sensec.sh stop"
		else
      		   export R_HOME="/usr/lib/R"
		   erl -noshell -pa ebin/ lib/*/ebin/ lib/*/bin/ -boot start_sasl -s reloader -s engine -sname engine > logs/"$now"/sensor-cloud_log.log &
		   echo $! > .temp.log
		fi
	 elif [ "$1" = "stop" ]; then
		echo "Closing down RabbitMQ-Server"
		./lib/rabbitmq-server/scripts/rabbitmqctl stop
		echo "Closing down ElasticSearch"
		./lib/elasticsearch/bin/service/elasticsearch stop
		echo "Closing Sensor-Cloud"
		kill -15 $(cat .temp.log)
		rm .temp.log
	fi
else 
	echo "$USER has no root access, use sudo"
fi


