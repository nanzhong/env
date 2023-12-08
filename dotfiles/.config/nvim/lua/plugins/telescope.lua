return {
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
          layout_config = {
            vertical = {
              prompt_position = 'top',
            },
            horizontal = {
              prompt_position = 'top',
            },
          },
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
      vim.keymap.set('n', '<leader>fs', require('telescope.builtin').grep_string, { desc = 'Search for string under cursor' })
      vim.keymap.set('n', '<leader>fb', require('telescope.builtin').buffers, { desc = 'Open buffer' })
      vim.keymap.set('n', '<leader>fh', require('telescope.builtin').help_tags, { desc = 'Open help tag' })
      vim.keymap.set('n', '<leader>gc', require('telescope.builtin').git_commits, { desc = 'Show git commits' })
      vim.keymap.set('n', '<leader>gcb', require('telescope.builtin').git_bcommits, { desc = 'Show buffer git commits' })
      vim.keymap.set('n', '<leader>gb', require('telescope.builtin').git_branches, { desc = 'Show git branches' })
      vim.keymap.set('n', '<leader>gs', require('telescope.builtin').git_status, { desc = 'Show git status' })
      vim.keymap.set('n', '<leader>le', require('telescope.builtin').diagnostics, { desc = 'Show diagnostics' })
      vim.keymap.set('n', '<leader>ld', require('telescope.builtin').lsp_definitions, { desc = 'Show definitions' })
      vim.keymap.set('n', '<leader>ldd', require('telescope.builtin').lsp_type_definitions, { desc = 'Show type definitions' })
      vim.keymap.set('n', '<leader>li', require('telescope.builtin').lsp_implementations, { desc = 'Show implemenations' })
      vim.keymap.set('n', '<leader>lr', require('telescope.builtin').lsp_references, { desc = 'Show references' })
    end
  },
  {
    'nvim-telescope/telescope-fzf-native.nvim',
    dependencies = { 'nvim-telescope/telescope.nvim' },
    build = 'make',
  },
  'nvim-telescope/telescope-ui-select.nvim',
}
