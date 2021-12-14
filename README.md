# env

This repository contains all the configuration needed to bootstrap my various machines and environments. It relies heavily on the nix ecosystem. This configuration manages my daily drivers that I use for both work @ [DigitalOcean](https://grnh.se/qmyvxul81) and for personal side projects, as well as the other machines that I use.

I mainly work on an iPad Pro connected to VMs that run this setup. Because it's tailored specifically to me, it likely contains things that are irrelevant or unwanted for you and your needs.

## Machines

| Hostname | System          | Description                                |
|----------|-----------------|--------------------------------------------|
| `wrk`    | `x86_64-linux`  | My workstation for DigitalOcean.           |
| `dev`    | `x86_64-linux`  | My workstation for personal side projects. |
| `media`  | `x86_64-linux`  | My media box.                              |
| `homepi` | `aarch64-linux` | My rpi at home.                            |


## Usage

To build and activate the various configurations

```sh
nixos-rebuild switch --flake <path to repo>#<hostname>
```
