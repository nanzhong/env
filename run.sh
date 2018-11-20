#!/bin/sh

docker run -it \
       -h workstation \
       -e TZ=America/Toronto \
       -v /root:/root/host \
       -v /var/run/docker.sock:/var/run/docker.sock \
       nanzhong/workstation
