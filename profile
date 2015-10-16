# Set up aliases and paths, different profile files"
source "$HOME/.paths"
source "$HOME/.aliases"

# This is necessary when we use urxvt, which is not recognized by e.g. tmux
export TERM='screen'

# More options
export EDITOR='vim'

# Mac OS X has a super low default for open files, this is annoying when
# benchmarking and stuff.
ulimit -n 8192

# Max OS X doesn't set LC_ALL correctly either
export LC_ALL="en_US.UTF-8"
