#!/bin/sh

set -e

echo "Attach to existing tmux session if it exists..."
/usr/bin/tmux attach || true

echo "Checking for host volume..."
if [ ! -d /mnt/host ]; then
    echo "/mnt/host not attached!"
    exit 1
fi

echo "Checking if keybase is needed for keys..."
if [ ! -d /mnt/shared/.ssh ] || [ ! -d /mnt/shared/.gnupg ]; then
    # TODO actually verify that keybase is running instead of blindly sleeping
    run_keybase && sleep 3
    keybase oneshot
fi

echo "Configuring ssh keys..."
rm -rf ~/.ssh
if [ ! -d /mnt/shared/.ssh ]; then
    keybase fs cp -r /keybase/private/nan/.ssh /mnt/shared/.
fi
ln -s /mnt/shared/.ssh ~/.

echo "Configuring gpg keys..."
rm -rf ~/.gnupg
if [ ! -d /mnt/shared/.gnupg ]; then
    homedir=/mnt/shared/.gnupg
    mkdir $homedir
    echo "Importing gpg keys from keybase..."
    keybase pgp export -s | gpg --homedir $homedir --batch --import
    echo "Modify trust on key..."
    gpg --homedir $homedir --edit-key nan@notanumber.io
fi
ln -s /mnt/shared/.gnupg ~/.

if [ -d /mnt/shared/src/org ]; then
    ln -s /mnt/shared/src/org ~/org
fi

echo "Configure kubectl..."
if [ ! -d /mnt/shared/.kube ]; then
    mkdir /mnt/shared/.kube
fi
ln -s /mnt/shared/.kube ~/.kube

if [ ! -d /mnt/shared/src ]; then
    echo "Configure src..."
    ln -s /mnt/shared/src ~/src
fi

echo "Starting tmux session..."
/usr/bin/tmux new
