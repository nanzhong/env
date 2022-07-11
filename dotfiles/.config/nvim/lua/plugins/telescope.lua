require('packer').use {
  'nvim-telescope/telescope.nvim',
  requires = { 'nvim-lua/plenary.nvim' },
  config = function ()
    vim.keymap.set('n', '<Leader>ff', require('telescope.builtin').find_files)
    vim.keymap.set('n', '<Leader>fg', require('telescope.builtin').live_grep)
    vim.keymap.set('n', '<Leader>fb', require('telescope.builtin').buffers)
    vim.keymap.set('n', '<Leader>fh', require('telescope.builtin').help_tags)
  end
}

require('packer').use {
	'nvim-telescope/telescope-fzf-native.nvim',
	requires = { 'nvim-telescope/telescope.nvim' },
	run = 'make',
	config = function ()
		require('telescope').load_extension('fzf')
	end
}
