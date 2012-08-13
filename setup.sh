#/bin/bash

function setup() {
    SRC="$1"
    DST="$2"
    echo "Installing $SRC..."

    mkdir -p $(dirname "$DST")
    ln -sfn "$PWD/$SRC" "$DST"
}

setup ackrc     "$HOME/.ackrc"
setup gitconfig "$HOME/.gitconfig"
setup vimrc     "$HOME/.vimrc"
setup xmonad.hs "$HOME/.xmonad/xmonad.hs"
