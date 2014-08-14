#!/bin/bash
set -o nounset -o errexit -o pipefail

# Virtual home
VHOME="$HOME"
if [[ -d /betteros ]]; then
    VHOME="/betteros/dotfiles/jasper.van.der.jeugt"
fi

# Utility: setup a single file
function setup() {
    SRC="$1"
    DST="$2"
    echo "Installing $SRC..."

    mkdir -p $(dirname "$DST")
    ln -sfn "$PWD/$SRC" "$DST"
}

# Utility: compile and setup a Haskell script
function setup-hs() {
    SRC="$1"
    DST="$2"
    EXE="$(echo "$SRC" | sed 's/\.[^.]*$//')"
    ghc --make -O2 -threaded "$SRC"
    setup "$EXE" "$DST"
}

# Copy configuration files
setup Xdefaults              "$VHOME/.Xdefaults"
setup ackrc                  "$VHOME/.ackrc"
setup aliases                "$VHOME/.aliases"
setup gitconfig              "$VHOME/.gitconfig"
setup msmtprc                "$VHOME/.msmtprc"
setup mutt/accounts/better   "$VHOME/.mutt/accounts/better"
setup mutt/accounts/personal "$VHOME/.mutt/accounts/personal"
setup muttrc                 "$VHOME/.muttrc"
setup offlineimaprc          "$VHOME/.offlineimaprc"
setup paths                  "$VHOME/.paths"
setup ssh-config             "$VHOME/.ssh/config"
setup vimrc                  "$VHOME/.vimrc"
setup xmonad.hs              "$VHOME/.xmonad/xmonad.hs"
setup zshrc                  "$VHOME/.zshrc"

# Copy binaries
setup bin/cabal-make           "$VHOME/.bin/cabal-make"
setup bin/id3v2-from-filenames "$VHOME/.bin/id3v2-from-filenames"
setup bin/decrypt-password     "$VHOME/.bin/decrypt-password"
setup bin/watermark-photo      "$VHOME/.bin/watermark-photo"

# Setup haskell scripts
setup-hs bin/raw-check.hs "$VHOME/.bin/raw-check"

# Create backup dir for vim
echo 'Creating vim backup dir...'
mkdir -p "$VHOME/.vim/backup"

# Install vundle if necessary
echo 'Installing vundle...'
if [[ ! -d "$VHOME/.vim/bundle/vundle" ]]; then
    git clone 'https://github.com/gmarik/vundle.git' "$VHOME/.vim/bundle/vundle"
    vim -c BundleInstall -c qa # Not sure if this is a nice way to do things
fi
