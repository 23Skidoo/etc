# -*- shell-script -*-

if [ ! "$CYGWIN" ]
then
    # Misc settings
    export PYTHONSTARTUP=/home/gman/.python
    export BC_ENV_ARGS='-q -l /home/gman/misc/extensions.bc'
    export PAGER=most

    # PATH-related
    export BIN_DIR=$HOME/bin
    export PATH=$PATH:$BIN_DIR:$HOME/.cabal/bin
    export PATH=$PATH:$BIN_DIR/ghc-7.0.4/bin
    export PATH=$PATH:$BIN_DIR/oasis-0.2.0/bin:$HOME/.godi/bin:$HOME/.godi/sbin
fi

export EDITOR="emacsclient -t -a vim"
export INFOPATH=$HOME/misc/info:

if [ ! "$CYGWIN" ]
then
    # Set up EC2
    source $HOME/misc/ec2.sh

    # export EC2_HOME=$HOME/.ec2
    # export EC2_PRIVATE_KEY=$EC2_HOME/pk-KEYID.pem
    # export EC2_CERT=$EC2_HOME/cert-CERTID.pem
    # export EC2_URL=https://ec2.eu-west-1.amazonaws.com
fi
