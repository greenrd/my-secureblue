#! /bin/bash -peux
export LANG=en_GB.UTF-8
pkg=sort-yaml-seqs
cd $pkg
stack test
stack install
install ~/.local/bin/${pkg}-exe /usr/bin