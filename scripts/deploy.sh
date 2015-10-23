#!/bin/bash -e

# Create tramp user.

adduser test
sudo -u test mkdir -p /home/test/.ssh
cat $HOME/.ssh/id_rsa.pub | sudo -u test cat > /home/test/.ssh/authorized_keys

# Register tramp host.

ssh-keygen -t rsa -b 4096 -f $HOME/.ssh/id_rsa -N ''
touch $HOME/.ssh/known_hosts
ssh-keygen -R localhost
ssh-keyscan -H localhost > $HOME/.ssh/known_hosts
