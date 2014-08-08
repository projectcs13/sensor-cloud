#!/bin/bash
### Author : Tommy Mattsson
### Purpose: Easy installation of needed software for our project that is part of the linux system rather than our own project
echo "#################################################################"
echo "Installing misc dependencies"
echo "#################################################################"
sudo apt-get install -yq xsltproc software-properties-common

echo "#################################################################"
echo "Installing Erlang"
echo "#################################################################"
wget http://packages.erlang-solutions.com/site/esl/esl-erlang/FLAVOUR_1_general/esl-erlang_16.b.1~ubuntu~precise_amd64.deb
sudo dpkg -i esl-erlang_16.b.1~ubuntu~precise_amd64.deb
rm esl-erlang_16.b.1~ubuntu~precise_amd64.deb

echo "#################################################################"
echo "Installing Nodejs together with npm"
echo "#################################################################"
sudo apt-get install python-software-properties python g++ make  
sudo add-apt-repository ppa:chris-lea/node.js  
sudo apt-get update -q
sudo apt-get install -yq nodejs  

echo "#################################################################"
echo "Installing R"
echo "#################################################################"
sudo add-apt-repository "deb http://ftp.sunet.se/pub/lang/CRAN/bin/linux/ubuntu precise/"
sudo apt-get update -q
sudo apt-get install -yq r-base
