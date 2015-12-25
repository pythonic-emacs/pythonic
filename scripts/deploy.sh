#!/bin/bash -e

if [ -z "$TRAVIS_BUILD_DIR" ]
then
    PROJECT_ROOT=$TRAVIS_BUILD_DIR
else
    PROJECT_ROOT=/vagrant
fi

# Create tramp user.

TRAMP_USER=test
TRAMP_HOME=/home/$TRAMP_USER

sudo useradd --home $TRAMP_HOME --create-home --user-group $TRAMP_USER

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
                        automake texinfo make git

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

# Install cask.

CASK_DIR=$HOME/.cask

if [ -d $CASK_DIR ]
then
    cd $CASK_DIR
    git pull
else
    git clone https://github.com/cask/cask $CASK_DIR
fi

cd $PROJECT_ROOT

export PATH=$PATH:$HOME/.cask/bin:$EMACS_DIR/emacs-24.3/bin:$EMACS_DIR/emacs-24.4/bin:$EMACS_DIR/emacs-24.5/bin

for VERSION in ${EMACS_VERSIONS[@]}
do
    EMACS=$VERSION cask install
    EMACS=$VERSION cask update
done

# Copy bashrc.

cp $PROJECT_ROOT/scripts/bashrc $HOME/.bashrc
