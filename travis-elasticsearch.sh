#!/usr/bin/env bash

download() {
    echo "Downloading elasticsearch from $1..."
    curl -s $1 | tar xz
    echo "Downloaded"
}

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
    cd $1/bin
    if [ $DEBUG ]
    then
        ./elasticsearch -f
    else
        ./elasticsearch 2>&1 /dev/null
    fi
    wait_for_elasticsearch
    cd ../../
    echo "Started"
}

#TODO
#post_some_documents() {
    
#}


download_and_run() {
    url="http://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-$1.tar.gz"
    dir_name="elasticsearch-$1"

    download $url

    # copies custom configurations

    #for file in $ELASTICSEARCH_CONFS
    #do
    #    if [ -f $file ]
    #    then
    #        cp $file $dir_name/example/elasticsearch/conf/
    #        echo "Copied $file into elasticsearch conf directory."
    #    fi
    #done

    # Run elasticsearch
    run $dir_name

    # Post documents
    #if [ -z "$ELASTICSEARCH_DOCS" ]
    #then
    #    echo "Indexing some default documents"
    #    post_some_documents $dir_name $dir_name/example/exampledocs/books.json
    #else
    #    echo "Indexing $ELASTICSEARCH_DOCS"
    #    post_some_documents $dir_name $ELASTICSEARCH_DOCS
    #fi
}

check_version() {
    case $1 in
        0.90.5|0.20.1|0.20.0|0.19.11);;
        *)
            echo "Sorry, $1 is not supported or not valid version."
            exit 1
            ;;
    esac
}


check_version $ELASTICSEARCH_VERSION
download_and_run $ELASTICSEARCH_VERSION
