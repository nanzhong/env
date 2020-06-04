alias n="nvim"
alias e="emacsclient"

if status is-interactive
  set -gx EDITOR 'emacsclient -t'
  set -gx VISUAL 'emacsclient -t'
end

eval (direnv hook fish)

if not set -q GOPATH
    set -gx GOPATH $HOME/go
end
if not set -q N_PREFIX
    set -gx N_PREFIX $HOME/.n
end

set -U fish_user_paths $HOME/bin $HOME/.fzf/bin $HOME/.rbenv/bin $GOPATH/bin $N_PREFIX/bin /usr/local/bin

status --is-interactive; and source (rbenv init -|psub)
