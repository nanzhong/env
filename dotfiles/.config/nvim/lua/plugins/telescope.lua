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

    vim.keymap.set('n', '<leader>ff', project_files)
    vim.keymap.set('n', '<leader>fg', require('telescope.builtin').live_grep)
    vim.keymap.set('n', '<leader>fs', require('telescope.builtin').grep_string)
    vim.keymap.set('n', '<leader>fb', require('telescope.builtin').buffers)
    vim.keymap.set('n', '<leader>fh', require('telescope.builtin').help_tags)
    vim.keymap.set('n', '<leader>gc', require('telescope.builtin').git_commits)
    vim.keymap.set('n', '<leader>gcb', require('telescope.builtin').git_bcommits)
    vim.keymap.set('n', '<leader>gb', require('telescope.builtin').git_branches)
    vim.keymap.set('n', '<leader>gs', require('telescope.builtin').git_status)
    vim.keymap.set('n', '<leader>le', require('telescope.builtin').diagnostics)
    vim.keymap.set('n', '<leader>ld', require('telescope.builtin').lsp_definitions)
    vim.keymap.set('n', '<leader>ldd', require('telescope.builtin').lsp_type_definitions)
    vim.keymap.set('n', '<leader>li', require('telescope.builtin').lsp_implementations)
    vim.keymap.set('n', '<leader>lr', require('telescope.builtin').lsp_references)
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
