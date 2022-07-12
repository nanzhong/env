require('packer').use {
  'nvim-telescope/telescope.nvim',
  requires = { 'nvim-lua/plenary.nvim' },
  config = function ()
    require('telescope').setup({
      pickers = {
        find_files = {
          hidden = true,
          mappings = {
            n = {
              ["cd"] = function(prompt_bufnr)
                local selection = require("telescope.actions.state").get_selected_entry()
                local dir = vim.fn.fnamemodify(selection.path, ":p:h")
                require("telescope.actions").close(prompt_bufnr)
                vim.cmd(string.format("silent cd %s", dir))
              end
            }
          }
        }
      }
    })
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
