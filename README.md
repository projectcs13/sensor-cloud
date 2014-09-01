# IoT-Framework Engine

Welcome to the "Project CS 2013" project

## Demo

You can check out a demo of the IoT-Framework here: [IoT-Framework demo](https://vimeo.com/98966770).

## Installing the project

1. Download and compile the linux system dependencies, (only needed once per machine)

   	make install_linux_deps

2. Download and compile the project dependencies, and compile the project sources

        make install

## Running the project

1. Run the application by using startup script (one of the commands below)

        make run_all
        sudo ./scripts/sensec.sh start

2. Alternative run (type each in separate shells)

        make run_rabbit
        make run_es
        make run_nodejs
        % don't forget to export R_HOME for example
        export R_HOME="/usr/lib/R"
        make run

4. To shutdown either close each individual shell or run one of the commands below

        make stop_all
        sudo ./scripts/sensec.sh stop

## Running tests

1. There are two ways of setting up the environment for testing. Either run the startup script by one of the below commands.

        make test_setup
        sudo ./scripts/sensec.sh test_setup

2. Or run each of the following commands in a separate shell

        make run_rabbit
        make run_es
        make run_nodejs
        make run_fake_resource

3. Run the tests

        make test

## Code Status

[![Build Status](https://travis-ci.org/projectcs13/sensor-cloud.png)](https://travis-ci.org/projectcs13/sensor-cloud)
