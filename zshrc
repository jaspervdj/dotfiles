# Set up aliases and paths, different profile files"
source "$HOME/.paths"
source "$HOME/.aliases"
if [[ -f "$HOME/.profile_ec2" ]]; then source "$HOME/.profile_ec2"; fi

# This is necessary when we use urxvt, which is not recognized by e.g. tmux
export TERM='xterm'

# More options
export EDITOR='vim'

# Enable command completion
autoload -U compinit
compinit

# Short function to get the current git branch, if available.
function ps1-git-branch() {
    if [[ -d "$PWD/.git" ]]; then
        echo "[$(git symbolic-ref --short HEAD)]"
    fi
}

# Prompt
setopt prompt_subst
export PS1='%F{yellow}%~%f%F{green}$(ps1-git-branch)%f$ '

# Source profile if available
if [[ -f "$HOME/.profile" ]]; then
    source "$HOME/.profile"
fi
