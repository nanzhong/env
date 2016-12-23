# editor
set EDITOR 'nvim'
set VISUAL 'nvim'

set -U fish_user_paths $HOME/bin $HOME/.cargo/bin $HOME/.rbenv/bin $HOME/.fzf/bin $HOME/go/bin /usr/local/bin /usr/local/sbin

# go
set -x GOPATH $HOME/go

# rbenv
status --is-interactive; and . (rbenv init -|psub)

# ls colors
set -x CLICOLOR 2
set -x LSCOLORS GxFxCxDxBxegedabagaced

# direnv
eval (direnv hook fish)
