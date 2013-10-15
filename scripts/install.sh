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

### Install Emacs erlang-mode
sudo apt-get install erlang-mode