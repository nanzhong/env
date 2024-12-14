alias nu-open = open
alias open = ^open

def --env darwin-rebuild-switch [target] {
  darwin-rebuild switch --flake $".#($target)" --print-build-logs
  reload-nix-env
}

$env.VISUAL = 'code --wait'
$env.EDITOR = 'code --wait'
$env.COLORTERM = 'truecolor'
$env.FZF_DEFAULT_COMMAND = 'fd --hidden .'
$env.FZF_DEFAULT_OPTS = '--cycle --layout=reverse --border --height=90% --preview-window=wrap --marker="*" --color="bg+:#1e2529,border:#272f35,gutter:#1e2529"'
$env.TERMINFO_DIRS = $'($env.HOME)/.terminfo ($env.TERMINFO_DIRS)'

$env.PATH = (
  $env.PATH
  | split row (char esep)
  | prepend ($env.HOME | path join go bin)
  | prepend ($env.HOME | path join bin)
  | uniq # filter so the paths are unique
)

$env.config = {
  show_banner: false,  
  buffer_editor: 'code --wait',
  

  hooks: {
    pre_prompt: [{ ||
      if (which direnv | is-empty) {
        return
      }

      direnv export json | from json | default {} | load-env
      if 'ENV_CONVERSIONS' in $env and 'PATH' in $env.ENV_CONVERSIONS {
        $env.PATH = do $env.ENV_CONVERSIONS.PATH.from_string $env.PATH
      }
    }]
  }
}

source ~/.cache/carapace/init.nu
source ~/.cache/zoxide.nu
