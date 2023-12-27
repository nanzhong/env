alias n="nvim"
alias e="emacsclient"

set -gx COLORTERM 'truecolor'

if status is-interactive
  set -gx EDITOR 'emacsclient -t'
  set -gx VISUAL 'emacsclient -c'
  set -gx FZF_DEFAULT_COMMAND 'fd --hidden .'
  set -gx FZF_DEFAULT_OPTS '--cycle --layout=reverse --border --height=90% --preview-window=wrap --marker="*" --color="bg+:#1e2529,border:#272f35,gutter:#1e2529"'
  set -gx FORGIT_FZF_DEFAULT_OPTS $FZF_DEFAULT_OPTS
  set -gx TERMINFO_DIRS $HOME/.terminfo $TERMINFO_DIRS
end

if type -q direnv
  eval (direnv hook fish)
end

if type -q go
  set -U fish_user_paths $HOME/bin (go env GOPATH)/bin
else
  set -U fish_user_paths $HOME/bin
end

if type -q zoxide
  zoxide init fish | source
end
