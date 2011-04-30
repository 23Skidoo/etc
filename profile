# -*- shell-script -*-

if [ ! "$CYGWIN" ]
then
    # Misc settings
    export PYTHONSTARTUP=/home/gman/.python
    export BC_ENV_ARGS='-q -l /home/gman/misc/extensions.bc'
    export PAGER=most

    # PATH-related
    export PATH=$PATH:/home/gman/bin:/home/gman/.cabal/bin
    export PATH=$PATH:/home/gman/bin/ghc-7.0.3/bin
fi

export EDITOR="emacsclient -t -a vim"
export INFOPATH=~/home/misc/info:

if [ ! "$CYGWIN" ]
then
    # Start the Emacs daemon
    /home/gman/bin/emacs-start
fi
