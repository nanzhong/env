return {
  {
    'rktjmp/lush.nvim',
    lazy = false,
    priority = 100,
    config = function()
      require('lush_theme.nan')
      vim.cmd [[colorscheme nan]]
    end
  },
  {
    'norcalli/nvim-colorizer.lua',
    config = function()
      require('colorizer').setup {
        '*',
        css = {
          rgb_fn = true,
          hsl_fn = true
        },
        html = {
          names = false
        }
      }
    end
  },
  {
    'lukas-reineke/indent-blankline.nvim',
    main = 'ibl',
    opts = {
      indent = {
        char = '│',
        highlight = 'IndentGuide',
      },
    },
  },
  {
    'lewis6991/gitsigns.nvim',
    config = function()
      require('gitsigns').setup()
    end
  },
  {
    'nvim-telescope/telescope.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      require('telescope').setup({
        pickers = {
          find_files = {
            hidden = true,
          },
        },
        defaults = {
          layout_strategy = 'flex',
        },
      })

      local function project_files()
        local opts = {}
        local ok = pcall(require('telescope.builtin').git_files, opts)
        if not ok then require('telescope.builtin').find_files(opts) end
      end

      vim.keymap.set('n', '<leader>ff', project_files, { desc = 'Open project file' })
      vim.keymap.set('n', '<leader>fa', require('telescope.builtin').find_files, { desc = 'Open file' })
      vim.keymap.set('n', '<leader>fg', require('telescope.builtin').live_grep, { desc = 'Search for string' })
      vim.keymap.set('n', '<leader>fs', require('telescope.builtin').grep_string,
        { desc = 'Search for string under cursor' })
      vim.keymap.set('n', '<leader>fb', require('telescope.builtin').buffers, { desc = 'Open buffer' })
      vim.keymap.set('n', '<leader>fh', require('telescope.builtin').help_tags, { desc = 'Open help tag' })
      vim.keymap.set('n', '<leader>fgc', require('telescope.builtin').git_commits, { desc = 'Show git commits' })
      vim.keymap.set('n', '<leader>fgcb', require('telescope.builtin').git_bcommits, { desc = 'Show buffer git commits' })
      vim.keymap.set('n', '<leader>fgb', require('telescope.builtin').git_branches, { desc = 'Show git branches' })
      vim.keymap.set('n', '<leader>fgs', require('telescope.builtin').git_status, { desc = 'Show git status' })
      vim.keymap.set('n', '<leader>fle', require('telescope.builtin').diagnostics, { desc = 'Show diagnostics' })
      vim.keymap.set('n', '<leader>fld', require('telescope.builtin').lsp_definitions, { desc = 'Show definitions' })
      vim.keymap.set('n', '<leader>fldd', require('telescope.builtin').lsp_type_definitions,
        { desc = 'Show type definitions' })
      vim.keymap.set('n', '<leader>fli', require('telescope.builtin').lsp_implementations,
        { desc = 'Show implemenations' })
      vim.keymap.set('n', '<leader>flr', require('telescope.builtin').lsp_references, { desc = 'Show references' })
    end
  },
  {
    'nvim-telescope/telescope-fzf-native.nvim',
    dependencies = { 'nvim-telescope/telescope.nvim' },
    build = 'make',
  },
  'nvim-telescope/telescope-ui-select.nvim',
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
              a = { bg = colours.base_0.hex, fg = colours.bg_1.hex, gui = 'bold' },
              b = { bg = colours.bg_2.hex, fg = colours.fg_1.hex },
              c = { bg = colours.bg_1.hex, fg = colours.fg_1.hex }
            },
            command = {
              a = { bg = colours.base_7.hex, fg = colours.bg_1.hex, gui = 'bold' },
              b = { bg = colours.bg_2.hex, fg = colours.fg_1.hex },
              c = { bg = colours.bg_1.hex, fg = colours.fg_1.hex }
            },
            inactive = {
              a = { bg = colours.bg_2.hex, fg = colours.fg_3.hex, gui = 'bold' },
              b = { bg = colours.bg_2.hex, fg = colours.fg_2.hex },
              c = { bg = colours.bg_1.hex, fg = colours.fg_2.hex }
            },
          },
          component_separators = { left = ' ', right = ' ' },
          section_separators = { left = ' ', right = ' ' },
        },
      }
    end
  },
  {
    'folke/which-key.nvim',
    event = "VeryLazy",
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
    end,
    opts = {},
  },
}
