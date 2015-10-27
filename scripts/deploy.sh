#!/bin/bash -e

HOST_USER=vagrant
HOST_HOME=/home/$HOST_USER
TRAMP_USER=test
TRAMP_HOME=/home/$TRAMP_USER

# Create tramp user.

adduser --quiet $TRAMP_USER

# Register tramp host.

sudo -u $HOST_USER ssh-keygen -t rsa -b 4096 -f $HOST_HOME/.ssh/id_rsa -N ''
sudo -u $HOST_USER touch $HOST_HOME/.ssh/known_hosts
sudo -u $HOST_USER ssh-keygen -R localhost
sudo -u $HOST_USER ssh-keyscan -H localhost > $HOST_HOME/.ssh/known_hosts

# Authorize localhost for tramp user.

sudo -u $TRAMP_USER mkdir -p $TRAMP_HOME/.ssh
sudo -u $HOST_USER cat $HOST_HOME/.ssh/id_rsa.pub | sudo -u $TRAMP_USER cat > $TRAMP_HOME/.ssh/authorized_keys
