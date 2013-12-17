# Sensor Cloud Engine

Welcome to the "Project CS 2013" project

## Running the project

1. Download and compile the dependencies, and compile the project sources

        make install

2. Run the application by using startup script

        sudo ./scripts/sensec.sh start

3. Alternative run (type each in separate shells)

        make run_es
        make run_rabbit
        make run_nodejs
        make run
        
4. To shutdown either close each individual shell or run

        sudo ./scripts/sensec.sh stop

## Running tests

1. Download and compile the dependencies, and compile the project sources

        make install
        
2. Start RabbitMQ

        make run_rabbit

3. Start Elastic Search server

        make run_es

4. Run the tests

        make test

## Code Status

[![Build Status](https://travis-ci.org/projectcs13/sensor-cloud.png)](https://travis-ci.org/projectcs13/sensor-cloud)
