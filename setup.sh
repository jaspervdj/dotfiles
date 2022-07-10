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

# Copy configuration files
setup Xresources             "$HOME/.Xresources"
setup ackrc                  "$HOME/.ackrc"
setup aliases                "$HOME/.aliases"
setup profile                "$HOME/.profile"
setup config.fish            "$HOME/.config/fish/config.fish"
setup gitconfig              "$HOME/.gitconfig"
setup kakrc                  "$HOME/.config/kak/kakrc"
setup kak-lsp.toml           "$HOME/.config/kak-lsp/kak-lsp.toml"
setup mailcap                "$HOME/.mailcap"
setup msmtprc                "$HOME/.msmtprc"
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
setup xmonad.hs              "$HOME/.xmonad/xmonad.hs"
setup alacritty.yml          "$HOME/.config/alacritty/alacritty.yml"

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
setup bin/disable-screensaver  "$HOME/.bin/disable-screensaver"
setup bin/mergy                "$HOME/.bin/mergy"
setup bin/minidlna.sh          "$HOME/.bin/minidlna.sh"
setup bin/rgrep                "$HOME/.bin/rgrep"
setup bin/note                 "$HOME/.bin/note"
