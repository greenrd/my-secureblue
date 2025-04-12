#! /bin/bash -peux
export LANG=en_GB.UTF-8
pkg=sort-yaml-seqs
cd $pkg
cabal update
cabal test
cabal install
install ~/.local/bin/${pkg}-exe /usr/bin