return {
	{
    'rktjmp/lush.nvim',
    config = function ()
      require('lush_theme.nan')
      vim.cmd[[colorscheme nan]]
    end
	}
}
