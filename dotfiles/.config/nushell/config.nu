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

  history: {
    file_format: sqlite
    max_size: 1_000_000
    sync_on_enter: true
    isolation: true
  }

  color_config: {
    binary: '#ac8f68'
    block: '#63e6e2'
    cell-path: '#f2f2f7'
    closure: '#40c8e0'
    custom: '#f2f2f7'
    duration: '#ff9d0a'
    float: '#ac8f68'
    glob: '#8e8e93'
    int: '#ac8f68'
    list: '#64d3ff'
    nothing: '#ff443a'
    range: '#ff9d0a'
    record: '#64d3ff'
    string: '#e5e5ea'

    bool: {|| if $in { '#64d3ff' } else { '#ff9d0a' } }

    date: {|| (date now) - $in |
        if $in < 1hr {
            { fg: '#ff443a' attr: 'b' }
        } else if $in < 6hr {
            '#ff443a'
        } else if $in < 1day {
            '#ff9d0a'
        } else if $in < 3day {
            '#ff9d0a'
        } else if $in < 1wk {
            '#30d158'
        } else if $in < 6wk {
            '#aeaeb2'
        } else if $in < 52wk {
            '#8e8e93'
        } else { '#48484a' }
    }

    filesize: {|e|
        if $e == 0b {
            '#48484a'
        } else if $e < 1mb {
            '#8e8e93'
        } else {{ fg: '#c7c7cc' }}
    }

    shape_and: { fg: '#ac8f68' attr: 'b' }
    shape_binary: { fg: '#ac8f68' attr: 'b' }
    shape_block: { fg: '#63e6e2' attr: 'b' }
    shape_bool: '#40c8e0'
    shape_closure: { fg: '#40c8e0' attr: 'b' }
    shape_custom: '#30d158'
    shape_datetime: { fg: '#40c8e0' attr: 'b' }
    shape_directory: '#40c8e0'
    shape_external: '#40c8e0'
    shape_external_resolved: '#40c8e0'
    shape_externalarg: { fg: '#30d158' attr: 'b' }
    shape_filepath: '#40c8e0'
    shape_flag: { fg: '#63e6e2' attr: 'b' }
    shape_float: { fg: '#ac8f68' attr: 'b' }
    shape_garbage: { fg: '#1c1c1e' bg: '#ff443a' attr: 'b' }
    shape_glob_interpolation: { fg: '#40c8e0' attr: 'b' }
    shape_globpattern: { fg: '#40c8e0' attr: 'b' }
    shape_int: { fg: '#ac8f68' attr: 'b' }
    shape_internalcall: { fg: '#40c8e0' attr: 'b' }
    shape_keyword: { fg: '#ac8f68' attr: 'b' }
    shape_list: { fg: '#40c8e0' attr: 'b' }
    shape_literal: '#63e6e2'
    shape_match_pattern: '#30d158'
    shape_matching_brackets: { attr: 'u' }
    shape_nothing: '#f43753'
    shape_operator: '#ff9d0a'
    shape_or: { fg: '#ac8f68' attr: 'b' }
    shape_pipe: { fg: '#ac8f68' attr: 'b' }
    shape_range: { fg: '#ff9d0a' attr: 'b' }
    shape_raw_string: { fg: '#e5e5ea' attr: 'b' }
    shape_record: { fg: '#40c8e0' attr: 'b' }
    shape_redirection: { fg: '#ac8f68' attr: 'b' }
    shape_signature: { fg: '#30d158' attr: 'b' }
    shape_string: '#e5e5ea'
    shape_string_interpolation: { fg: '#40c8e0' attr: 'b' }
    shape_table: { fg: '#63e6e2' attr: 'b' }
    shape_vardecl: { fg: '#63e6e2' attr: 'u' }
    shape_variable: '#ac8f68'

    foreground: '#f2f2f7'
    background: '#1c1c1e'
    cursor: '#f2f2f7'

    empty: '#8e8e93'
    header: { fg: '#30d158' attr: 'b' }
    hints: '#3a3a3c'
    leading_trailing_space_bg: { attr: 'n' }
    row_index: { fg: '#30d158' attr: 'b' }
    search_result: { fg: '#1c1c1e' bg: '#bf5af2' }
    separator: '#d1d1d6'
  }

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

source ~/.cache/starship/init.nu
source ~/.cache/carapace/init.nu
source ~/.cache/zoxide.nu
