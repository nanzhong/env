alias n="nvim"
alias e="emacsclient"

if status is-interactive
  set EDITOR 'e -t'
  set VISUAL 'e -t'
end

if not set -q GOPATH
    set -gx GOPATH $HOME/go
end
set -U fish_user_paths $HOME/bin $GOPATH/bin /usr/local/bin

source ~/.asdf/asdf.fish
eval (direnv hook fish)
