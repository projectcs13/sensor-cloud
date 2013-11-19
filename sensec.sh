#!/bin/bash

#Check if user has root access
if [ "$USER" = "root" ]; then
	curr_dir=`pwd`
	dir=`dirname $0`
	#Specify project home directory.
	HOME_PATH=`cd  $dir;pwd`
	#Specify log directory.
	LOG_DIR=$HOME_PATH/logs
	echo $HOME_PATH
	if [ "$1" = "start" ]; then
		#Create log directory if it does not already exist
		now="$(date +%d_%m_%Y_%H_%M_%S)"		
		mkdir -p $LOG_DIR/"$now"
		chmod 777 $LOG_DIR
		echo "Starting up RabbitMQ-Server"
		$HOME_PATH/lib/rabbitmq-server/scripts/rabbitmq-server > $LOG_DIR/"$now"/rabbit_log.log &
		echo "Starting up ElasticSearch"
		$HOME_PATH/lib/elasticsearch/bin/service/elasticsearch console > $LOG_DIR/"$now"/es_log.log &
		if [ -s $HOME_PATH/.temp.log ]; then
   		   echo "Sensor cloud already running, to close run ./sensec.sh stop"
		else
		   sleep 2
		   echo "Starting node.js with receive.js"
		   nodejs $HOME_PATH/javascripts/receive.js > $LOG_DIR/"$now"/nodejs_log.log &
		   echo $! > $HOME_PATH/.temp.log
		   echo "Starting Sensor-Cloud"
      	   export R_HOME="/usr/lib/R"
		   erl -noshell -env SENSOR_PATH $HOME_PATH -pa $HOME_PATH/ebin/ $HOME_PATH/lib/*/ebin/ $HOME_PATH/lib/*/bin/ -boot start_sasl -s reloader -s engine -sname engine -config $HOME_PATH/config/engine.config > $LOG_DIR/"$now"/sensor-cloud_log.log &
		   echo $! >> $HOME_PATH/.temp.log
		fi
	 elif [ "$1" = "stop" ]; then
		echo "Closing nodejs and Sensor-Cloud"
		while read line
		do
			kill -15 "$line"
		done < $HOME_PATH/.temp.log
		rm $HOME_PATH/.temp.log

		echo "Closing down RabbitMQ-Server"
		$HOME_PATH/lib/rabbitmq-server/scripts/rabbitmqctl stop
		echo "Closing down ElasticSearch"
		$HOME_PATH/lib/elasticsearch/bin/service/elasticsearch stop
	fi
else 
	echo "$USER has no root access, use sudo"
fi


