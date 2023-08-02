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
setup gitconfig-fugue        "$HOME/.gitconfig-fugue"
setup gitconfig-snyk         "$HOME/.gitconfig-snyk"
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
setup pypanelrc              "$HOME/.pypanelrc"
setup ssh-config             "$HOME/.ssh/config"
setup tmux.conf              "$HOME/.tmux.conf"
setup xmonad.hs              "$HOME/.xmonad/xmonad.hs"
setup alacritty.yml          "$HOME/.config/alacritty/alacritty.yml"

# Copy "binaries"
setup bin/decrypt-password     "$HOME/.local/bin/decrypt-password"
setup bin/monitor              "$HOME/.local/bin/monitor"
setup bin/phototools           "$HOME/.local/bin/phototools"
setup bin/terminal             "$HOME/.local/bin/terminal"
setup bin/volume               "$HOME/.local/bin/volume"
setup bin/wallpaper            "$HOME/.local/bin/wallpaper"
setup bin/colorprof            "$HOME/.local/bin/colorprof"
setup bin/unicode              "$HOME/.local/bin/unicode"
setup bin/disable-screensaver  "$HOME/.local/bin/disable-screensaver"
setup bin/mergy                "$HOME/.local/bin/mergy"
setup bin/rgrep                "$HOME/.local/bin/rgrep"
setup bin/note                 "$HOME/.local/bin/note"
