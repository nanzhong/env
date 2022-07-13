require('packer').use {
  'nvim-telescope/telescope.nvim',
  requires = { 'nvim-lua/plenary.nvim' },
  config = function ()
    require('telescope').setup({
      pickers = {
        find_files = {
          hidden = true
        }
      }
    })

    local function project_files()
      local opts = {}
      local ok = pcall(require('telescope.builtin').git_files, opts)
      if not ok then require('telescope.builtin').find_files(opts) end
    end

    vim.keymap.set('n', '<Leader>ff', project_files)
    vim.keymap.set('n', '<Leader>fg', require('telescope.builtin').live_grep)
    vim.keymap.set('n', '<Leader>fs', require('telescope.builtin').grep_string)
    vim.keymap.set('n', '<Leader>fb', require('telescope.builtin').buffers)
    vim.keymap.set('n', '<Leader>fh', require('telescope.builtin').help_tags)
    vim.keymap.set('n', '<Leader>gc', require('telescope.builtin').git_commits)
    vim.keymap.set('n', '<Leader>gcb', require('telescope.builtin').git_bcommits)
    vim.keymap.set('n', '<Leader>gb', require('telescope.builtin').git_branches)
    vim.keymap.set('n', '<Leader>gs', require('telescope.builtin').git_status)
    vim.keymap.set('n', '<Leader>ld', require('telescope.builtin').diagnostics)
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
