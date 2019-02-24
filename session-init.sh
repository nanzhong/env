#!/bin/sh

set -e

echo "Attach to existing tmux session if it exists..."
/usr/bin/tmux attach || true

echo "Checking for host volume..."
if [ ! -d ~/host ]; then
    echo "~/host not attached!"
    exit 1
fi

echo "Checking if keybase is needed for keys..."
if [ ! -d ~/host/root/.ssh ] || ! ls ~/host/root/.ssh/id_* > /dev/null 2>&1 || [ ! -d ~/host/root/.gnupg ]; then
    # TODO actually verify that keybase is running instead of blindly sleeping
    run_keybase && sleep 3
    keybase oneshot
fi

echo "Configuring ssh keys..."
rm -rf ~/.ssh
if [ -d ~/host/root/.ssh ] && ls ~/host/root/.ssh/id_* > /dev/null 2>&1; then
    cp -a ~/host/root/.ssh ~/.
else
    keybase fs cp -r /keybase/private/nan/.ssh ~/.
fi

echo "Configuring gpg keys..."
rm -rf ~/.gnupg
if [ -d ~/host/root/.gnupg ]; then
    cp -a ~/host/root/.gnupg ~/.
else
    echo "Importing gpg keys from keybase..."
    keybase pgp export -s | gpg --batch --import
    echo "Modify trust on key..."
    gpg --edit-key nan@notanumber.io
fi

if [ -d ~/host/root/src/org ]; then
    ln -s ~/host/root/src/org ~/org
fi

echo "Configure kubectl + minikube..."
if [ -d ~/host/root/.kube ]; then
    ln -s ~/host/root/.kube ~/.kube
fi
if [ -d ~/host/root/.minikube ]; then
    ln -s ~/host/root/.minikube ~/.minikube
fi

echo "Starting tmux session..."
/usr/bin/tmux new
