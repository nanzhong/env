require('packer').use {
  'akinsho/toggleterm.nvim',
  tag = 'v2.*',
  config = function()
    require('toggleterm').setup {
      direction = 'float',
      open_mapping = '<leader>`',
      insert_mappings = false,
      float_opts = {
        border = 'rounded',
      }
    }

    local Terminal  = require('toggleterm.terminal').Terminal
    local lazygit = Terminal:new({
      cmd = 'lazygit',
      hidden = true,
    })

    vim.keymap.set('n', '<leader>g', function() lazygit:toggle() end, { silent = true, desc = 'Toggle lazygit window' })
  end
}

