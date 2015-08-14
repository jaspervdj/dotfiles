# Set up aliases and paths, different profile files"
source "$HOME/.paths"
source "$HOME/.aliases"
if [[ -f "$HOME/.profile_ec2" ]]; then source "$HOME/.profile_ec2"; fi

# This is necessary when we use urxvt, which is not recognized by e.g. tmux
export TERM='screen'

# More options
export EDITOR='vim'

# Enable command completion
autoload -U compinit
compinit

# Short function to get the current git branch, if available.
function ps1-git-branch() {
  # If inside a git dir...
  if git rev-parse 2>/dev/null; then
    echo "[$(git symbolic-ref --short HEAD 2>/dev/null || echo 'no branch')]"
  fi
}

# Prompt
setopt prompt_subst
export PS1='%F{yellow}%~%f%F{green}$(ps1-git-branch)%f$ '

# Source profile if available
if [[ -f "$HOME/.profile" ]]; then
  source "$HOME/.profile"
fi

# Mac OS X has a super low default for open files, this is annoying when
# benchmarking and stuff.
if [[ "$(uname)" == 'Darwin' ]]; then ulimit -n 8192; fi

# Max OS X doesn't set LC_ALL correctly either
if [[ "$(uname)" == 'Darwin' ]]; then export LC_ALL="en_US.UTF-8"; fi

# Init ruby
eval "$(rbenv init -)"
