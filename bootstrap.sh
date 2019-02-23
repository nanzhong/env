#!/bin/sh

set -e

echo "Setting hostanme..."
hostnamectl set-hostname "workstation"

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
        software-properties-common \
        openconnect

curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add
# use buster because no repo exists for sid
add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/debian buster stable"

apt-get update && \
    apt-get install -qy \
            build-essential \
            git \
            mosh \
            jq \
            docker-ce=18.06.1~ce~3-0~debian \
            libvirt-clients \
            libvirt-daemon-system \
            qemu-kvm \
            dnsutils

echo "Fetching kubernetes dependencies..."
curl -LO https://storage.googleapis.com/kubernetes-release/release/v1.13.3/bin/linux/amd64/kubectl && chmod +x kubectl && mv kubectl /usr/local/bin/kubectl
curl -Lo minikube https://storage.googleapis.com/minikube/releases/v0.34.1/minikube-linux-amd64 && chmod +x minikube && mv minikube /usr/local/bin/
curl -LO https://storage.googleapis.com/minikube/releases/latest/docker-machine-driver-kvm2 && install docker-machine-driver-kvm2 /usr/local/bin/ && rm docker-machine-driver-kvm2

echo "Configure custom ip block for docker..."
echo '{"bip":"172.24.0.1/24","fixed-cidr":"172.24.0.0/24"}' > /etc/docker/daemon.json
systemctl restart docker

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

echo "Add my ssh keys..."
curl -s https://api.github.com/users/nanzhong/keys | jq -r .[].key | while read key
do
  echo $key >> /root/.ssh/authorized_keys
done
