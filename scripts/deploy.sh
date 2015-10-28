#!/bin/bash -e

PROJECT_ROOT=/vagrant

# Create tramp user.

TRAMP_USER=test
TRAMP_HOME=/home/$TRAMP_USER

sudo adduser --quiet $TRAMP_USER

# Register tramp host.

ssh-keygen -t rsa -b 4096 -f $HOME/.ssh/id_rsa -N ''
touch $HOME/.ssh/known_hosts
ssh-keygen -R localhost
ssh-keyscan -H localhost > $HOME/.ssh/known_hosts

# Authorize localhost for tramp user.

sudo -u $TRAMP_USER mkdir -p $TRAMP_HOME/.ssh
sudo -u $TRAMP_USER sh -c "echo '$(cat $HOME/.ssh/id_rsa.pub)' > $TRAMP_HOME/.ssh/authorized_keys"

# Update mirrors list.

sudo apt-get update

# Install Emacs build dependencies.

sudo apt-get install -y libncurses-dev libxpm-dev libxaw7-dev \
                        libtiff4-dev libpng-dev libgif-dev autoconf \
                        automake texinfo make

# Build Emacs.

EMACS_DIR=$HOME/.emacs
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
