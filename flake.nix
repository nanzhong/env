{
  description = "Nan's environment configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, ... }@inputs:
    with inputs;
    let
      mkSystem = pkgs: system: machine:
        pkgs.lib.nixosSystem {
          system = system;
          modules = builtins.attrValues self.nixosModules ++ [
            (./machines + "/${machine}/configuration.nix")
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
              };

              nixpkgs.overlays = [
                inputs.emacs-overlay.overlay
              ];
            }
          ];
          specialArgs = { inherit inputs; };
        };
    in {
      nixosModules = builtins.listToAttrs (map (m: {
        name = m;
        value = import (./modules + "/${m}");
      }) (builtins.attrNames (builtins.readDir ./modules)));

      nixosConfigurations = {
        wrk = mkSystem inputs.nixpkgs "x86_64-linux" "wrk";
        dev = mkSystem inputs.nixpkgs "x86_64-linux" "dev";
        media = mkSystem inputs.nixpkgs "x86_64-linux" "media";
      };
    };
}
