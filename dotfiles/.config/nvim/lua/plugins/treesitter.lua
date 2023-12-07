return {
  {
    'nvim-treesitter/nvim-treesitter',
    dev = true,
    config = function ()
      require('nvim-treesitter.configs').setup {

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
