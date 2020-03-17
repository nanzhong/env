# Workstation

My portable development environment.

This repository contains scripts to build a docker container that holds my development environment as well as scripts to bootstrap a new machine to serve as a container host.

![Unique](unique.jpg)

This is my daily driver that I use for both work @ [DigitalOcean](https://grnh.se/qmyvxul81) and for personal side projects. I work on an iPad Pro connected to a droplet that runs this setup. Because it's tailored specifically to me, it may contain things that are irrelevant or unwanted for you and your needs.

## Features

- Based on Archlinux
- Fish shell 3.0
- Mosh
- Tmux
- Emacs
- Language support for:
  - Golang
  - Ruby
  - NodeJS
- Kubectl
- Keybase
- Globalprotect VPN (client + routes to preserve connection)

## Usage

### Setting up the docker host

Provision your docker host machine however you want, and run:
```
./boostrap.sh
```

_Note: The `boostrap.sh` script assumes Debian._

### Building the docker container

Nothing special is needed to build the container, just:

```
make build
```

### Running the container

Included in the repo is a `run.sh` script that is an example of how the container can be run.
