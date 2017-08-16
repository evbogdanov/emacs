#!/usr/bin/env bash

mkdir bak
rm -rf $HOME/.emacs.d
ln -s $HOME/github/emacs/ $HOME/.emacs.d
