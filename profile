# -*- shell-script -*-

# Misc settings
export PYTHONSTARTUP=$HOME/.python
export BC_ENV_ARGS="-q -l $HOME/misc/extensions.bc"
export PAGER=most
export RUST_SRC_PATH=$HOME/code/rust/rust/src
export CARGO_HOME=$HOME/.cargo

# Set up PATH.
export BIN_DIR=$HOME/bin
export PATH=$PATH:/opt/ghc/bin
export PATH=$PATH:$BIN_DIR:$HOME/.cabal/bin
export PATH=$PATH:$HOME/.cargo/bin/
export PATH=$PATH:/opt/cabal/1.24/bin
export PATH=$PATH:/opt/alex/3.1.7/bin
export PATH=$PATH:/opt/happy/1.19.5/bin

# Set up some common dir aliases.
export CODE_DIR=~/code
export HASKELL_DIR=$CODE_DIR/haskell
export CABAL_ROOT_DIR=$HASKELL_DIR/cabal
export CABAL_DIR=$CABAL_ROOT_DIR/Cabal
export CABAL_INSTALL_DIR=$CABAL_ROOT_DIR/cabal-install
export GHC_DIR=$HASKELL_DIR/ghc
export GHC_OBJ_DIR=$HASKELL_DIR/ghc-obj

export EDITOR="emacsclient -t -a zile"
export INFOPATH=$HOME/misc/info:
