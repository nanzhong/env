require('packer').use {
  'TimUntersberger/neogit',
  requires = 'nvim-lua/plenary.nvim',
  config = function ()
    local neogit = require('neogit')
    neogit.setup {}

    vim.keymap.set('n', '<leader>g', neogit.open, { silent = true, desc = 'Open neogit' })
  end
}
