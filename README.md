# Workstation

My development environment.

This repository contains all the configuration needed to bootstrap my full development environment. It relies heavily on the nix ecosystem. This is my daily driver that I use for both work @ [DigitalOcean](https://grnh.se/qmyvxul81) and for personal side projects. I work on an iPad Pro connected to VMs that run this setup. Because it's tailored specifically to me, it may contain things that are irrelevant or unwanted for you and your needs.

![Unique](unique.jpg)

## Usage

### nixos

First determine the machine that needs to be bootstrapped and configure the `current.nix` symlink. 
```
ln -s ./wrk.nix ./nix/machines/current.nix
```

Create the symlink for `/etc/nixos/configuration.nix`.
```
ln -s (pwd -P)/nix/configuration.nix /etc/nixos/configuration.nix
```

### nix-darwin

TODO
