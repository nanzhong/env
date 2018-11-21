#!/bin/sh

docker run -it \
       -h workstation.docker \
       -e TZ=America/Toronto \
       -v /:/root/host \
       -v /var/run/docker.sock:/var/run/docker.sock \
       --detach-keys "ctrl-^,d" \
       nanzhong/workstation
