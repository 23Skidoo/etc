#! /bin/sh

# Borrowed from jaspervdj's dotfiles.
function setup() {
    SRC="$1"
    DST="$2"
    echo "Installing $SRC..."

    mkdir -p $(dirname "$DST")
    ln -sfn "$PWD/$SRC" "$DST"
}

setup conkyrc   "$HOME/.conkyrc"
setup ghci      "$HOME/.ghci"
setup gitconfig "$HOME/.gitconfig"
setup profile   "$HOME/.profile"
setup python    "$HOME/.python"
setup zile      "$HOME/.zile"
setup zshrc     "$HOME/.zshrc"
