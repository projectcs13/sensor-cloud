#!/bin/bash
### Author : Tommy Mattsson
### Purpose: Easy installation of needed software for our project that is part of the linux system rather than our own project



echo "#################################################################"
echo "Installing Erlang"
echo "#################################################################"
wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb
rm -f erlang-solutions_1.0_all.deb

wget http://packages.erlang-solutions.com/debian/erlang_solutions.asc
sudo apt-key add erlang_solutions.asc
rm -f erlang_solutions.asc

sudo apt-get update
sudo apt-get install erlang

### Install Emacs erlang-mode
sudo apt-get install erlang-mode