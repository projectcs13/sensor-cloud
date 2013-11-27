#!/bin/bash
dir=`dirname $0`
#Specify project home directory.
HOME_PATH=`cd  $dir;cd ..;pwd`
LOG_DIR=$HOME_PATH/priv/logs
LOG_JS_DIR=$LOG_DIR

echo "$HOME_PATH"
#Check if user has root access
if [ "$USER" = "root" ]; then
	if [ "$1" = "start" ]; then	
		echo "Starting up RabbitMQ-Server"
		$HOME_PATH/lib/rabbitmq-server/scripts/rabbitmq-server &
		echo "Starting up ElasticSearch"
		$HOME_PATH/lib/elasticsearch/bin/service/elasticsearch console &
		if [ -s $HOME_PATH/.temp.log ]; then
   		   echo "Sensor cloud already running, to close run ./sensec.sh stop"
		else
		   sleep 5
		   echo "Starting node.js with receive.js"
		   if [ -d "$LOG_JS_DIR" ]; then
				nodejs $HOME_PATH/javascripts/receive.js > $LOG_JS_DIR/nodejs_log.log &
		   else
		    	nodejs $HOME_PATH/javascripts/receive.js &
		   fi
		   echo $! > $HOME_PATH/.temp.log
		   echo "Starting Sensor-Cloud"
      	   export R_HOME="/usr/lib/R"
      	   if [ -d "$LOG_DIR" ]; then
		   		erl -noshell -env SENSOR_PATH $HOME_PATH -pa $HOME_PATH/ebin/ $HOME_PATH/lib/*/ebin/ $HOME_PATH/lib/*/bin/ -boot start_sasl -s reloader -s engine -sname engine -config $HOME_PATH/config/engine.config > $LOG_DIR/sensor-cloud_log.log &
		   else
		   		erl -noshell -env SENSOR_PATH $HOME_PATH -pa $HOME_PATH/ebin/ $HOME_PATH/lib/*/ebin/ $HOME_PATH/lib/*/bin/ -boot start_sasl -s reloader -s engine -sname engine -config $HOME_PATH/config/engine.config &
		   fi
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


