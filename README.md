# env

This repository contains all the configuration needed to bootstrap my various machines and environments. It relies heavily on the nix ecosystem. This configuration manages my daily drivers that I use for both work and personal side projects, as well as the other machines that I use. This setup is tailored specifically to me, so it likely contains things that are irrelevant or unwanted for you and your needs.

While I am by no means a nix expert, and don't pretend to be one, I aim to follow best practices and to demonstrate one way to organize and structure system and user configuration using a nix flake.

The general layout I use is as follows:
```
.
├─ bin/                               - scripts and other executables
│  └─ ...
├─ dotfiles/                          - user configuration
│  └─ ...
├─ machines/                          - system configuration
│  ├─ dev
│  │  ├─ configuration.nix            - main
│  │  ├─ hardware-configuration.nix   - hardware specific, usually for VMs (optional)
│  │  └─ networking.nix               - net specific, usually for VMs (optional)
│  └─ ...
├─ modules/                           - reusable nix modules
│  ├─ common                          - modules are organized by folder
│  │  ├─ darwin.nix                   - macos specific (optional, imports default.nix)
│  │  ├─ default.nix                  - main
│  │  └─ nixos.nix                    - linux specific (optional, imports default.nix)
│  └─ ...
├─ overlays/                          - nix overlays
│  └─ ...
└─ patches/                           - custom patches
   └─ ...
```

## Quirks

### I don't use nix to manage by dotfiles directly

I only make use of home-manager to create symlinks for my dotfiles. The user configuration itself is just plain old regular files. The two main reason I do this are:
- My dotfiles are also directly usable in non-nix environments
- To not be constrained by what options the nix version of the config supports

## Machines

| Hostname | System           | Description                                     |
|----------|------------------|-------------------------------------------------|
| `dev`    | `x86_64-linux`   | Cloud Workstation vm for personal side projects |
| `media`  | `x86_64-linux`   | Cloud media box                                 |
| `homepi` | `aarch64-linux`  | rpi at home                                     |
| `devpi`  | `aarch64-linux`  | rpi I carry around for fun                      |
| `stdio`  | `aarch64-darwin` | M2 Ultra Mac Studio - personal                  |
| `wrk`    | `aarch64-darwin` | M4 Pro Macbook Pro - work                       |