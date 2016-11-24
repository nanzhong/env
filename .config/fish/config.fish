set -U fish_user_paths $HOME/bin $fish_user_paths

# editor
set EDITOR 'nvim'
set VISUAL 'nvim'

# homebrew
set -U fish_user_paths /usr/local/bin /usr/local/sbin $fish_user_paths

# go
set -x GOPATH $HOME/go
set -U fish_user_paths $HOME/go/bin $fish_user_paths

# rbenv
set -U fish_user_paths $HOME/.rbenv/bin $fish_user_paths
status --is-interactive; and . (rbenv init -|psub)

# ls colors
set -x CLICOLOR 2
set -x LSCOLORS GxFxCxDxBxegedabagaced
