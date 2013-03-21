# Set up aliases and paths
source "$HOME/.paths"
source "$HOME/.aliases"

# Enable command completion
autoload -U compinit
compinit

# Prompt
export PS1='%F{yellow}%~%f$ '
