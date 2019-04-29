# Workstation

My portable development environment.

This repository contains scripts to build a docker container that holds my development environment as well as scripts to bootstrap a new machine to serve as a container host.

![Unique](unique.jpg)

This is my daily driver that I use for both work @ [DigitalOcean](https://grnh.se/qmyvxul81) and for personal side projects. I work on an iPad Pro connected to a droplet that runs this setup. Because it's tailored specifically to me, it may contain things that are irrelevant or unwanted for you and your needs.

## Features

- Based on Ubuntu 18.04
- Fish shell 3.0
- Mosh
- Tmux 2.9
- Emacs 26.2
- Language support for:
  - Golang
  - Ruby
  - Python
- Ready for Kubernetes development (Kubectl)
- Keybase
- Globalprotect VPN (client + routes to preserve connection)

## Usage

### Setting up the docker host

Provision your docker host machine however you want, and run:
```
./boostrap.sh
```

_Note: The `boostrap.sh` script assumes Ubuntu 18.04._

### Building the docker container

Nothing special is needed to build the container, just:

```
docker build -t <name> .
```

### Running the container

Included in the repo is a `run.sh` script that is an example of how the container can be run.

## Todo

- [ ] Pull out things specific to me into configurable/optional parts
  - [ ] authorized keys in bootstrap.sh
  - [ ] globalprotect vpn client
  - [ ] keybase related ssh and gpg key management specific to me
- [ ] update `run.sh` to support flags for:
  - [ ] conditionally use host network
  - [ ] conditionally mount host docker socket
