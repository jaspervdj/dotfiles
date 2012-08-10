#/bin/bash

function setup() {
    SRC="$1"
    DST="$2"
    echo "Installing $1..."

    mkdir -p $(dirname "$DST")
    ln "$SRC" "$DST"
}

setup vimrc     "$HOME/.vimrc"
setup xmonad.hs "$HOME/.xmonad/xmonad.hs"
