# aliases
function em
	emacsclient $argv
end

alias em emacsclient

# editor
set EDITOR 'em -n'
set VISUAL 'em -n'

# rbenv
set PATH $HOME/bin $HOME/.rbenv/bin $PATH
status --is-interactive; and . (rbenv init -|psub)

# go
set -x GOPATH $HOME/go
set PATH $GOPATH/bin $PATH

# swift
setenv SWIFTENV_ROOT "$HOME/.swiftenv"
setenv PATH "$SWIFTENV_ROOT/bin" $PATH
status --is-interactive; and . (swiftenv init -|psub)
