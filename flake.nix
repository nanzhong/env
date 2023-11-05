{
  description = "Nan's environment configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:nixos/nixos-hardware/master";

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    mkAlias = {
      url = "github:reckenrode/mkAlias";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    zig = {
      url = "github:mitchellh/zig-overlay";
    };
  };

  outputs = { self, darwin, ... }@inputs:
    with inputs;
    let
      commonModules = {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
        };

        nixpkgs.overlays = [
          inputs.emacs-overlay.overlay
          inputs.zig.overlays.default
        ];
      };
      mkLinuxSystem = pkgs: system: machine:
        pkgs.lib.nixosSystem {
          system = system;
          modules = builtins.attrValues self.nixosModules ++ [
            (./machines + "/${machine}/configuration.nix")
            home-manager.nixosModules.home-manager
            commonModules
          ];
          specialArgs = { inherit inputs; };
        };

      mkDarwinSystem = pkgs: system: machine:
        darwin.lib.darwinSystem {
          system = system;
          modules = builtins.attrValues self.darwinModules ++ [
            (./machines + "/${machine}/configuration.nix")
            home-manager.darwinModules.home-manager
            commonModules
          ];
          specialArgs = { inherit inputs system; };
        };
    in {
      nixosModules = builtins.listToAttrs (map (m: {
        name = m;
        value = import (./modules/nixos + "/${m}");
      }) (builtins.attrNames (builtins.readDir ./modules/nixos)));
      darwinModules = builtins.listToAttrs (map (m: {
        name = m;
        value = import (./modules/darwin + "/${m}");
      }) (builtins.attrNames (builtins.readDir ./modules/darwin)));

      nixosConfigurations = {
        wrk = mkLinuxSystem inputs.nixpkgs "x86_64-linux" "wrk";
        dev = mkLinuxSystem inputs.nixpkgs "x86_64-linux" "dev";
        media = mkLinuxSystem inputs.nixpkgs "x86_64-linux" "media";
        homepi = mkLinuxSystem inputs.nixpkgs "aarch64-linux" "homepi";
      };

      darwinConfigurations = {
        stdio = mkDarwinSystem inputs.nixpkgs "aarch64-darwin" "stdio";
        do = mkDarwinSystem inputs.nixpkgs "aarch64-darwin" "do";
      };
    };
}
