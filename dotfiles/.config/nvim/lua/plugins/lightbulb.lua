require('packer').use {
  'kosayoda/nvim-lightbulb',
  requires = 'antoinemadec/FixCursorHold.nvim',
  config = function()
    vim.fn.sign_define('LightBulbSign', { text = '⨀', texthl = 'LightBulbSign' })

    require('nvim-lightbulb').setup({
      autocmd = {
        enabled = true
      }
    })
  end
}
