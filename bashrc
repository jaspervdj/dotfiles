#
# ~/.bashrc
#

################################################################################
# Default stuff
################################################################################

# If not running interactively, don't do anything
# [[ $- != *i* ]] && return
# PS1='[\u@\h \W]\$ '
PS1='> '

################################################################################
# Various variables
################################################################################

export EDITOR="kak"
export GOPATH="$HOME/.go"
export NMON="lmnDt"

################################################################################
# Other configurations
################################################################################

source "$HOME/.paths"
source "$HOME/.aliases"

################################################################################
# Node
################################################################################

export NPM_PACKAGES="$HOME/.npm-packages"
export PATH="$NPM_PACKAGES/bin:$PATH"
export NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"

################################################################################
# Man path
################################################################################

unset MANPATH  # delete if you already modified MANPATH elsewhere in your configuration
export MANPATH="$NPM_PACKAGES/share/man:$(manpath)"
