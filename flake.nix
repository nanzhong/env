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

    neovim = {
      url = "github:neovim/neovim/master?dir=contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    mkAlias = {
      url = "github:reckenrode/mkAlias";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    zig = {
      url = "github:mitchellh/zig-overlay";
    };

    zls = {
      url = "github:zigtools/zls";
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      mkLinuxSystem = pkgs: system: machine:
        pkgs.lib.nixosSystem {
          system = system;
          modules = [
            ./modules/common/nixos.nix
            ./modules/dev/nixos.nix
            ./modules/home/nixos.nix
            (./machines + "/${machine}/configuration.nix")
          ];
          specialArgs = { inherit inputs; };
        };

      mkDarwinSystem = pkgs: system: machine:
        inputs.darwin.lib.darwinSystem {
          system = system;
          modules = [
            ./modules/common/darwin.nix
            ./modules/dev/darwin.nix
            ./modules/home/darwin.nix
            (./machines + "/${machine}/configuration.nix")
          ];
          specialArgs = { inherit inputs system; };
        };

      forEachSystem = fn:
        nixpkgs.lib.genAttrs [
          "x86_64-linux" "aarch64-linux"
          "x86_64-darwin" "aarch64-darwin"
        ] (system: fn {
          pkgs = import nixpkgs { inherit system; };
        });
    in {
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

      devShells = forEachSystem ({ pkgs, ... }: {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            lua-language-server
          ];
        };
      });
    };
}
