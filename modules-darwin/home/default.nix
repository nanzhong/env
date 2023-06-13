{ lib, config, pkgs, ... }:
with lib;
let
  keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIvUFgn/Sa929gXPYRlQcnmJtFG/1ntas0q1ShlzmKPt nan@iphone"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPRVyHqcSWD8nhiniAfDlV3UIua0/mkINp1XbmcwGHVc nan@ipad"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDH6q/qDHWgll9yMvxdbiBGKL/o6vp6ZfV17ckOKXozK nan@dev"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIh99gRWj1Lot63fO+XB4z+YRttqFDh4SHnTD80XTope nan@wrk"
  ];
  cfg = config.nanzhong.home;
in {
  imports = [ ../../modules/home/default.nix ];
  config = {
    users.users = {
      "${cfg.user}" = {
        home = "/Users/${cfg.user}";
        shell = pkgs.fish;
        openssh.authorizedKeys.keys = keys;
      };
    };
  };
}
