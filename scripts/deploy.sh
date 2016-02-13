#!/bin/bash -e

# Install evm.

if [ ! -d $HOME/.evm ]
then
    git clone https://github.com/rejeep/evm $HOME/.evm
else
    git -C $HOME/.evm pull
fi

EVM_DIR=$HOME/.evm/versions

mkdir -p $EVM_DIR

PATH=$HOME/.evm/bin:$HOME/.cask/bin:$PATH

evm config path $EVM_DIR

evm install emacs-24.3 --skip
evm install emacs-24.4 --skip
evm install emacs-24.5 --skip
evm install emacs-git-snapshot --skip

# Install cask.

if [ ! -d $HOME/.cask ]
then
    git clone https://github.com/cask/cask $HOME/.cask
else
    git -C $HOME/.cask pull
fi

evm use emacs-24.3
cask --path /vagrant/ install
cask --path /vagrant/ update

evm use emacs-24.4
cask --path /vagrant/ install
cask --path /vagrant/ update

evm use emacs-24.5
cask --path /vagrant/ install
cask --path /vagrant/ update

evm use emacs-git-snapshot
cask --path /vagrant/ install
cask --path /vagrant/ update

# Copy bashrc.

cp /vagrant/script/bashrc $HOME/.bashrc
