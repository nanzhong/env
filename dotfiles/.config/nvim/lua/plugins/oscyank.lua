require('packer').use {
  'ojroques/vim-oscyank',
  config = function ()
    vim.keymap.set('n', '<leader>o', '<plug>(OSCYank)', { desc = 'Yank selection via OSC52' })
  end
}
