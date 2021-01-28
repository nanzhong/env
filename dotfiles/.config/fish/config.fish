alias n="nvim"
alias e="emacsclient"

if status is-interactive
  set -gx EDITOR 'emacsclient -t'
  set -gx VISUAL 'emacsclient -t'
end

eval (direnv hook fish)

set -U fish_user_paths $HOME/bin (go env GOPATH)/bin

starship init fish | source

source (z --init fish enhanced | psub)
alias zz="z -c"
alias zi="z -i"
alias zf="z -I"
alias zb="z -b"
