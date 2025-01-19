#! /bin/bash -peux
cd /usr/local
curl -L --silent 'https://github.com/mifi/lossless-cut/releases/latest/download/LosslessCut-linux-x64.tar.bz2'|tar xvfj -
mkdir bin
cd bin
ln -s ../LosslessCut-linux-x64/losslesscut