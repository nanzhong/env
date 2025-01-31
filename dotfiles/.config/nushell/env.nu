use NIX_BASH_ENV_NU_MODULE
def --env reload-nix-env [--reset] {
  if $reset {
    hide-env --ignore-errors __NIXOS_SET_ENVIRONMENT_DONE
    hide-env --ignore-errors __NIX_DARWIN_SET_ENVIRONMENT_DONE
  }
  bash-env /etc/bashrc | load-env
}
reload-nix-env

$env.CARAPACE_BRIDGES = 'zsh,fish,bash,inshellisense' # optional
mkdir ~/.cache/carapace
carapace _carapace nushell | save --force ~/.cache/carapace/init.nu

zoxide init nushell | save --force ~/.cache/zoxide.nu

mkdir ~/.cache/starship
starship init nu | save -f ~/.cache/starship/init.nu