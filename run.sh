#!/bin/sh

docker run -it \
       -h workstation.docker \
       -e TZ=America/Toronto \
       -e KEYBASE_USERNAME=nan \
       -v /:/mnt/host \
       -v /mnt/shared:/mnt/shared \
       -v /var/run/docker.sock:/var/run/docker.sock \
       --detach-keys "ctrl-^,d" \
       --privileged \
       --net host \
       --name workstation \
       --rm \
       --ulimit memlock=67108864 \
       nanzhong/workstation:latest
