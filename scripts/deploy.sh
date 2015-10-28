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

# Update mirrors list.

apt-get update

# Install Emacs build dependencies.

apt-get install -y libncurses-dev libxpm-dev libxaw7-dev \
                   libtiff4-dev libpng-dev libgif-dev autoconf \
                   automake texinfo make

# Build Emacs.

EMACS_DIR=/usr/local/emacs
EMACS_SRC=$EMACS_DIR/src
EMACS_VERSIONS=(emacs-24.3 emacs-24.4 emacs-24.5)

mkdir -p $EMACS_DIR $EMACS_SRC

for VERSION in ${EMACS_VERSIONS[@]}
do
    FILE=${VERSION}.tar.xz
    PREFIX=$EMACS_DIR/$VERSION
    cd $EMACS_SRC
    wget -q http://ftp.gnu.org/gnu/emacs/$FILE
    tar xvJf $FILE
    cd $VERSION
    ./configure --prefix=$PREFIX
    make
    make install
    rm $PREFIX/bin/emacs
done
