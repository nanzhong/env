require('packer').use {
  'TimUntersberger/neogit',
  requires = 'nvim-lua/plenary.nvim',
  config = function ()
    local neogit = require('neogit')
    neogit.setup {
      disable_commit_confirmation = true,
      disable_insert_on_commit = false,
      kind = 'floating',
      commit_popup = {
        kind = 'floating',
      },
      signs = {
        section = { '▶', '▼' },
        item = { '▶', '▼' },
        hunk = { '', '' },
      },
    }

    vim.keymap.set('n', '<leader>g', neogit.open, { silent = true, desc = 'Open neogit' })
  end
}
