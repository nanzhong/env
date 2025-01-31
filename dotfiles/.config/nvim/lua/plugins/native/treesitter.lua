return {
  {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    config = function ()
      require('nvim-treesitter.configs').setup {
        ensure_installed = {
          'bash',
          'comment',
          'css',
          'dockerfile',
          'fish',
          'gitattributes',
          'gitignore',
          'go',
          'gomod',
          'gowork',
          'hcl',
          'javascript',
          'jq',
          'json',
          'lua',
          'make',
          'markdown',
          'nix',
          'python',
          'ruby',
          'rust',
          'toml',
          'typescript',
          'vue',
          'yaml',
          'zig',
        },
        highlight = {
          enable = true
        },
        indent = {
          enable = true
        },
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = 'gnn',
            node_incremental = 'grn',
            scope_incremental = 'grc',
            node_decremental = 'grm',
          },
        }
      }
    end
  }
}
