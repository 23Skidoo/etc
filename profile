# -*- shell-script -*-

if [ ! "$CYGWIN" ]
then
    # Misc settings
    export PYTHONSTARTUP=$HOME/.python
    export BC_ENV_ARGS="-q -l $HOME/misc/extensions.bc"
    export PAGER=most

    # PATH-related
    export BIN_DIR=$HOME/bin
    export PATH=$PATH:$BIN_DIR/sbt/bin
    export PATH=$PATH:$BIN_DIR:$HOME/.cabal/bin
    export PATH=$PATH:$BIN_DIR/ghc-7.4.2/bin
    export PATH=$PATH:$BIN_DIR/opam/bin
    eval `opam config -env`
fi

export EDITOR="emacsclient -t -a zile"
export INFOPATH=$HOME/misc/info:

if [ ! "$CYGWIN" ]
then
    # Set up EC2
    # export EC2_HOME=$HOME/.ec2
    # export EC2_PRIVATE_KEY=$EC2_HOME/pk-KEYID.pem
    # export EC2_CERT=$EC2_HOME/cert-CERTID.pem
    # export EC2_URL=https://ec2.eu-west-1.amazonaws.com
    source $HOME/misc/ec2.sh

    # Start the Emacs daemon
    $HOME/bin/emacs-start
fi
