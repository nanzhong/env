#!/bin/sh

set -e

echo "Attach to existing tmux session if it exists..."
/usr/bin/tmux attach || true

echo "Checking for host volume..."
if [ ! -d ~/host ]; then
    echo "~/host not attached!"
    exit 1
fi

echo "Checking ssh keys..."
if [ -d ~/host/.ssh ]; then
    mkdir -p ~/.ssh
    cp -a ~/host/.ssh/* ~/.ssh/.
else
    eval $(~/bin/op signin my.1password.com nan@notanumber.io)
    echo "Setting up ssh key..."
    mkdir -p ~/.ssh
    ~/bin/op get item "key.workstation.do" | jq -r .details.notesPlain > ~/.ssh/id_ed25519
    chmod 700 ~/.ssh
    chmod 600 ~/.ssh/id_ed25519
    ssh-keygen -yf ~/.ssh/id_ed25519 > ~/.ssh/id_ed25519.pub
    chmod 644 ~/.ssh/id_ed25519.pub
fi

if [ -d ~/host/src/org ]; then
    ln -s ~/host/src/org ~/org
fi

echo "Starting tmux session..."
/usr/bin/tmux new
