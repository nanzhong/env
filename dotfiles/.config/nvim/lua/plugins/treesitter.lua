require('packer').use {
	'nvim-treesitter/nvim-treesitter',
	run = ':TSUpdate',
	config = function ()
		require('nvim-treesitter.configs').setup {
			ensure_installed = 'all',
			highlight = {
				enable = true
			},
			indent = {
				enable = true
			},
			incremental_selection = {
				enable = true,
				keymaps = {
					init_selection = "gnn",
					node_incremental = "grn",
					scope_incremental = "grc",
					node_decremental = "grm",
				},
			}
		}
	end
}
