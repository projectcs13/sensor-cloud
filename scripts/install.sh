#!/bin/bash
### Author : Tommy Mattsson
### Purpose: Easy installation of needed software for our project that is part of the linux system rather than our own project
echo "#################################################################"
echo "Installing Erlang"
echo "#################################################################"
grep -q -e 'deb http://packages.erlang-solutions.com/debian '$(lsb_release -sc)' contrib' /etc/apt/sources.list || echo 'deb http://packages.erlang-solutions.com/debian '$(lsb_release -sc)' contrib' >> /etc/apt/sources.list
sudo wget http://packages.erlang-solutions.com/debian/erlang_solutions.asc
sudo apt-key add erlang_solutions.asc
rm -f erlang_solutions.asc

sudo apt-get update
sudo apt-get install -y erlang

echo "#################################################################"
echo "Installing Nodejs together with npm"
echo "#################################################################"
sudo apt-get install python-software-properties python g++ make  
sudo add-apt-repository ppa:chris-lea/node.js  
sudo apt-get update  
sudo apt-get install nodejs  
(cd javascripts; npm install socket.io; npm install rabbit.js)

echo "#################################################################"
echo "Installing R"
echo "#################################################################"
sudo add-apt-repository "deb http://ftp.sunet.se/pub/lang/CRAN/bin/linux/ubuntu precise/"
### If you don't have add-apt-repository you can get it via:

sudo apt-get install software-properties-common
###Then to do the actual installation:

sudo apt-get update
sudo apt-get install r-base

echo "#################################################################"
echo "Installing misc dependencies"
echo "#################################################################"
sudo apt-get install -qq xsltproc