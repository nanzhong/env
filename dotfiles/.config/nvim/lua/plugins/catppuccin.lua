require('packer').use {
  'catppuccin/nvim',
  as = 'catpuccin',
  config = function ()
    vim.g.catppuccin_flavour = "mocha"
    vim.cmd[[colorscheme catppuccin]]
  end
} 
