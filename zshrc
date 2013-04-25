# Set up aliases and paths
source "$HOME/.paths"
source "$HOME/.aliases"

# This is necessary when we use urxvt, which is not recognized by e.g. tmux
export TERM='xterm'

# More options
export EDITOR='vim'

# Enable command completion
autoload -U compinit
compinit

# Prompt
export PS1='%F{yellow}%~%f$ '

# Source profile if available
if [[ -f "$HOME/.profile" ]]; then
    source "$HOME/.profile"
fi
