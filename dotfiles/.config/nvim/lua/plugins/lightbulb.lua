return {
  {
    'kosayoda/nvim-lightbulb',
    dependencies = 'antoinemadec/FixCursorHold.nvim',
    config = function()
      vim.fn.sign_define('LightBulbSign', { text = '⨀', texthl = 'LightBulbSign' })
  
      require('nvim-lightbulb').setup({
        autocmd = {
          enabled = true
        }
      })
    end
  }
}
