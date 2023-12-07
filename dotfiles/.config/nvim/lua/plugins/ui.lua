return {
	{
    'rktjmp/lush.nvim',
		lazy = false,
		priority = 100,
    config = function ()
      require('lush_theme.nan')
      vim.cmd[[colorscheme nan]]
    end
	},
  {
    'nvim-lualine/lualine.nvim',
    lazy = false,
    dependencies = { 'rktjmp/lush.nvim' },
    opts = function()
      local colours = require('lush_theme.nan.colours')
      return {
        options = {
          theme = {
            normal = {
              a = { bg = colours.base_3.hex, fg = colours.bg_1.hex, gui = 'bold' },
              b = { bg = colours.bg_2.hex, fg = colours.fg_1.hex },
              c = { bg = colours.bg_1.hex, fg = colours.fg_1.hex }
            },
            insert = {
              a = { bg = colours.base_4.hex, fg = colours.bg_1.hex, gui = 'bold' },
              b = { bg = colours.bg_2.hex, fg = colours.fg_1.hex },
              c = { bg = colours.bg_1.hex, fg = colours.fg_1.hex }
            },
            visual = {
              a = { bg = colours.base_2.hex, fg = colours.bg_1.hex, gui = 'bold' },
              b = { bg = colours.bg_2.hex, fg = colours.fg_1.hex },
              c = { bg = colours.bg_1.hex, fg = colours.fg_1.hex }
            },
            replace = {
              a = {bg = colours.base_0.hex, fg = colours.bg_1.hex, gui = 'bold'},
              b = { bg = colours.bg_2.hex, fg = colours.fg_1.hex },
              c = { bg = colours.bg_1.hex, fg = colours.fg_1.hex }
            },
            command = {
              a = { bg = colours.base_7.hex, fg = colours.bg_1.hex, gui = 'bold'},
              b = { bg = colours.bg_2.hex, fg = colours.fg_1.hex },
              c = { bg = colours.bg_1.hex, fg = colours.fg_1.hex }
            },
            inactive = {
              a = { bg = colours.bg_2.hex, fg = colours.fg_3.hex, gui = 'bold' },
              b = { bg = colours.bg_2.hex, fg = colours.fg_2.hex },
              c = { bg = colours.bg_1.hex, fg = colours.fg_2.hex }
            },
          },
          component_separators = { left = ' ', right = ' '},
          section_separators = { left = ' ', right = ' '},
        },
      }
    end
  },
}
