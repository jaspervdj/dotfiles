# Set up paths correctly
source "$HOME/.paths"

# Enable command completion
autoload -U compinit
compinit

# Prompt
export PS1='%F{yellow}%~%f$ '
