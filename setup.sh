#!/bin/bash
set -o nounset -o errexit -o pipefail

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
    stack exec ghc -- --make -O2 -threaded "$SRC"
    setup "$EXE" "$DST"
}

# Copy configuration files
setup Xresources             "$HOME/.Xresources"
setup ackrc                  "$HOME/.ackrc"
setup aliases                "$HOME/.aliases"
setup config.fish            "$HOME/.config/fish/config.fish"
setup gitconfig              "$HOME/.gitconfig"
setup mailcap                "$HOME/.mailcap"
setup msmtprc                "$HOME/.msmtprc"
setup mutt/accounts/better   "$HOME/.mutt/accounts/better"
setup mutt/accounts/luminal  "$HOME/.mutt/accounts/luminal"
setup mutt/accounts/personal "$HOME/.mutt/accounts/personal"
setup muttrc                 "$HOME/.muttrc"
setup offlineimaprc          "$HOME/.offlineimaprc"
setup openbox/autostart      "$HOME/.config/openbox/autostart"
setup openbox/environment    "$HOME/.config/openbox/environment"
setup openbox/menu.xml       "$HOME/.config/openbox/menu.xml"
setup openbox/rc.xml         "$HOME/.config/openbox/rc.xml"
setup paths                  "$HOME/.paths"
setup pypanelrc              "$HOME/.pypanelrc"
setup ssh-config             "$HOME/.ssh/config"
setup tmux.conf              "$HOME/.tmux.conf"
setup vimrc                  "$HOME/.vimrc"
setup xmonad.hs              "$HOME/.xmonad/xmonad.hs"

# Copy "binaries"
setup bin/cabal-make           "$HOME/.bin/cabal-make"
setup bin/decrypt-password     "$HOME/.bin/decrypt-password"
setup bin/monitor              "$HOME/.bin/monitor"
setup bin/find-holes           "$HOME/.bin/find-holes"
setup bin/id3v2-from-filenames "$HOME/.bin/id3v2-from-filenames"
setup bin/phototools           "$HOME/.bin/phototools"
setup bin/terminal             "$HOME/.bin/terminal"
setup bin/volume               "$HOME/.bin/volume"
setup bin/rsnapshotter         "$HOME/.bin/rsnapshotter"
setup bin/wallpaper            "$HOME/.bin/wallpaper"
setup bin/colorprof            "$HOME/.bin/colorprof"
setup bin/unicode              "$HOME/.bin/unicode"
setup bin/tmux-resize-auto     "$HOME/.bin/tmux-resize-auto"

# Setup haskell scripts
setup-hs bin/raw-check.hs    "$HOME/.bin/raw-check"
setup-hs bin/sum-doubles.hs  "$HOME/.bin/sum-doubles"
setup-hs bin/obmenus.hs      "$HOME/.bin/obmenus"
setup-hs bin/markdown-toc.hs "$HOME/.bin/markdown-toc"

# Create backup dir for vim
echo 'Creating vim backup dir...'
mkdir -p "$HOME/.vim/backup"

# Install vundle if necessary
echo 'Installing vundle...'
if [[ ! -d "$HOME/.vim/bundle/Vundle.vim" ]]; then
    git clone 'https://github.com/VundleVim/Vundle.vim.git' "$HOME/.vim/bundle/Vundle.vim"
    vim -c VundleInstall -c qa # Not sure if this is a nice way to do things
fi
