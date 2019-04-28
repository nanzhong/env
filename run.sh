#!/bin/sh

docker run -it \
       -h workstation.docker \
       -e TZ=America/Toronto \
       -e KEYBASE_USERNAME=nan \
       -v /:/root/host \
       -v /root/src:/root/src \
       -v /var/run/docker.sock:/var/run/docker.sock \
       --detach-keys "ctrl-^,d" \
       --privileged
       --net host \
       --name workstation \
       --rm \
       nanzhong/workstation
