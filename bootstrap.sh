#!/bin/sh

set -e

echo "Setting hostanme..."
hostnamectl set-hostname "workstation"

echo "Updating system and install dependencies..."
apt-get update && \
    apt-get -qy install \
            apt-transport-https \
            ca-certificates \
            curl \
            gnupg-agent \
            software-properties-common

curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add
add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu bionic stable"

apt-get update && \
    apt-get install -qy \
            build-essential \
            git \
            mosh \
            jq \
            docker-ce \
            dnsutils \
            tigervnc-standalone-server firefox wmctrl

echo "Fetching kubernetes dependencies..."
curl -LO https://storage.googleapis.com/kubernetes-release/release/v1.14.1/bin/linux/amd64/kubectl && chmod +x kubectl && mv kubectl /usr/local/bin/kubectl

echo "Configure custom ip block for docker..."
echo '{"bip":"172.24.0.1/24","fixed-cidr":"172.24.0.0/24"}' > /etc/docker/daemon.json
systemctl restart docker

echo "Configure routes to preserve networking for vpn..."
ip=$(ip addr show eth0 | grep -oP '(?<=inet\s)\d+(\.\d+){3}')
subnet=$(ip route | grep -Po '^\d+(.\d+){3}/\d+(?= dev eth0)')
gateway=$(ip route | grep -Po '(?<=default via )[.\d]+')

echo "network:
  version: 2
  ethernets:
    eth0:
      routing-policy:
        - from: $ip
          table: 128
      routes:
        - to: $subnet
          via: $gateway
          table: 128
        - to: 0.0.0.0/0
          via: $gateway
          table: 128" > /etc/netplan/99-vpn.yaml
echo "128	mgmt" > /etc/iproute2/rt_tables.d/mgmt.conf
netplan apply

echo "Installing vpn..."
mkdir vpn && cd vpn
curl -Lo vpn.tgz https://security.nyc3.digitaloceanspaces.com/VPN/PanGPLinux-4.1.6-c3.tgz
tar -zxvf vpn.tgz
apt-get install -qy ./GlobalProtect_deb-4.1.6.0-3.deb
cd ../ && rm -rf vpn

echo "Add my ssh keys..."
curl -s https://api.github.com/users/nanzhong/keys | jq -r .[].key | while read key
do
  echo $key >> /root/.ssh/authorized_keys
done
