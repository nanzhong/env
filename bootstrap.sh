#!/bin/sh

set -e

echo "Setting hostanme..."
hostnamectl set-hostname "workstation"

echo "Add my ssh keys..."
curl -s https://api.github.com/users/nanzhong/keys | jq -r .[].key | while read key
do
  echo $key >> /root/.ssh/authorized_keys
done

echo "Upgrade to sid..."
echo "deb http://deb.debian.org/debian unstable main contrib non-free
deb-src http://deb.debian.org/debian unstable main contrib non-free" > /etc/apt/sources.list
apt-get update && apt-get -y dist-upgrade

echo "Updating system and install dependencies..."
apt-get -qy install \
        apt-transport-https \
        ca-certificates \
        curl \
        gnupg2 \
        software-properties-common

curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add
# use buster because no repo exists for sid
add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/debian buster stable"

apt-get update && \
    apt-get install -qy \
            build-essential \
            git \
            mosh \
            docker-ce \
            libvirt-clients \
            libvirt-daemon-system \
            qemu-kvm

echo "Fetching kubernetes dependencies..."
curl -LO https://storage.googleapis.com/kubernetes-release/release/v1.12.2/bin/linux/amd64/kubectl && chmod +x kubectl && mv kubectl /usr/local/bin/kubectl
curl -Lo minikube https://storage.googleapis.com/minikube/releases/v0.30.0/minikube-linux-amd64 && chmod +x minikube && mv minikube /usr/local/bin/
curl -LO https://storage.googleapis.com/minikube/releases/latest/docker-machine-driver-kvm2 && install docker-machine-driver-kvm2 /usr/local/bin/ && rm docker-machine-driver-kvm2

echo "Configure routes to preserve networking for vpn..."
ip=$(ip addr show eth0 | grep -oP '(?<=inet\s)\d+(\.\d+){3}')
subnet=$(ip route | grep -Po '^\d+(.\d+){3}/\d+(?= dev eth0)')
gateway=$(ip route | grep -Po '(?<=default via )[.\d]+')
ip rule add from $ip table 128 > /dev/null 2>&1 || true
ip route add table 128 to $subnet dev eth0 > /dev/null 2>&1 || true
ip route add table 128 default via $gateway > /dev/null 2>&1 || true

echo "128	mgmt" > /etc/iproute2/rt_tables.d/mgmt.conf
grep -q '# routing rules for vpn' /etc/network/interfaces || {
    echo "	# routing rules for vpn" >> /etc/network/interfaces
    echo "	post-up ip rule add from $ip table 128" >> /etc/network/interfaces
    echo "	post-up ip route add table 128 to $subnet dev eth0" >> /etc/network/interfaces
    echo "	post-up ip route add table 128 default via $gateway" >> /etc/network/interfaces
}

echo "Installing vpn..."
mkdir vpn && cd vpn
curl -Lo vpn.tgz https://security.nyc3.digitaloceanspaces.com/VPN/PanGPLinux-4.1.6-c3.tgz
tar -zxvf vpn.tgz
apt-get install -qy ./GlobalProtect_deb-4.1.6.0-3.deb
cd ../
# workaround for vpn cert issue
mkdir -p /home/yyin/opensource/openssl/openssl-1.0.1t-build/ssl/certs
ln -s /etc/ssl/certs/3513523f.0 /home/yyin/opensource/openssl/openssl-1.0.1t-build/ssl/certs/. > /dev/null 2>&1
