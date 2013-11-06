#!/usr/bin/env bash

is_elasticsearch_up(){
    http_code=`echo $(curl -s -o /dev/null -w "%{http_code}" "http://localhost:9200")`
    return `test $http_code = "200"`
}

wait_for_elasticsearch(){
    while ! is_elasticsearch_up; do
        sleep 3
    done
}

run() {
    echo "Starting elasticsearch ..."
    if [ $DEBUG ]
    then
        ./lib/elasticsearch/bin/elasticsearch -f
    else
        ./lib/elasticsearch/bin/elasticsearch 2>&1 /dev/null
    fi
    wait_for_elasticsearch
    cd ../../
    echo "Started"
}

run 
