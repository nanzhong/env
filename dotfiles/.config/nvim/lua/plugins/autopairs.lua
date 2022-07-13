require('packer').use {
  "windwp/nvim-autopairs",
  config = function ()
    require("nvim-autopairs").setup({})
  end
}
