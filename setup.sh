#!/bin/bash

function setup() {
    SRC="$1"
    DST="$2"
    echo "Installing $SRC..."

    mkdir -p $(dirname "$DST")
    ln -sfn "$PWD/$SRC" "$DST"
}

setup Xdefaults  "$HOME/.Xdefaults"
setup ackrc      "$HOME/.ackrc"
setup aliases    "$HOME/.aliases"
setup gitconfig  "$HOME/.gitconfig"
setup paths      "$HOME/.paths"
setup ssh-config "$HOME/.ssh/config"
setup vimrc      "$HOME/.vimrc"
setup xmonad.hs  "$HOME/.xmonad/xmonad.hs"
setup zshrc      "$HOME/.zshrc"

setup bin/cabal-make "$HOME/.bin/cabal-make"
