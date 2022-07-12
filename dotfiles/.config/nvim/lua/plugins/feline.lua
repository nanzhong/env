require('packer').use {
  'feline-nvim/feline.nvim',
  requires = { 'rktjmp/lush.nvim', 'kyazdani42/nvim-web-devicons' },
  config = function ()
    local hsl_to_hex = require('lush.hsl.convert').hsl_to_hex
    local colours = require('lush_theme.nan.colours')

    local left_banner = {
      provider = '█▓▒ ',
      hl = {
        fg = hsl_to_hex(colours.base_4)
      },
    }

    local vi_mode = {
      provider = 'vi_mode',
      hl = function()
        return {
          name = require('feline.providers.vi_mode').get_mode_highlight_name(),
          bg = require('feline.providers.vi_mode').get_mode_color(),
          fg = hsl_to_hex(colours.bg_2),
          style = 'bold'
        }
      end,
      opts = {
        padding = 'right',
      },
      icon = '',
      left_sep = '',
      right_sep = ''
    }

    local file_info = {
      provider = 'file_info',
      hl = {
        fg = hsl_to_hex(colours.white),
        style = 'bold',
      },
      opts = {
        file_modified_icon = '●',
        file_readonly_icon = '',
        type = 'unique'
      },
      left_sep = ' '
    }

    local lsp_client_names = {
      provider = 'lsp_client_names',
      hl = {
        fg = hsl_to_hex(colours.base_1),
      },
      left_sep = ' '
    }

    local lsp_diag_errors = {
      provider = 'diagnostic_errors',
      enabled = function ()
        require('feline.providers.lsp').diagnostics_exist(vim.diagnostic.severity.ERROR)
      end,
      hl = {
        fg = hsl_to_hex(colours.base_0)
      },
      icon = '⚑ ',
      left_sep = ' '
    }

    local lsp_diag_warnings = {
      provider = 'diagnostic_warnings',
      enabled = function ()
        require('feline.providers.lsp').diagnostics_exist(vim.diagnostic.severity.WARN)
      end,
      hl = {
        fg = hsl_to_hex(colours.base_2)
      },
      icon = '⚐ ',
      left_sep = ' '
    }

    local lsp_diag_hints = {
      provider = 'diagnostic_hints',
      enabled = function ()
        require('feline.providers.lsp').diagnostics_exist(vim.diagnostic.severity.HINT)
      end,
      hl = {
        fg = hsl_to_hex(colours.base_4)
      },
      icon = '☻ ',
      left_sep = ' '
    }

    local lsp_diag_info = {
      provider = 'diagnostic_info',
      enabled = function ()
        require('feline.providers.lsp').diagnostics_exist(vim.diagnostic.severity.INFO)
      end,
      hl = {
        fg = hsl_to_hex(colours.fg)
      },
      icon = '☺ ',
      left_sep = ' '
    }

    local git_branch = {
      provider = 'git_branch',
      hl = {
        fg = hsl_to_hex(colours.base_7)
      },
      icon = ' ',
      right_sep = ' '
    }

    local git_diff_added = {
      provider = 'git_diff_added',
      hl = {
        fg = hsl_to_hex(colours.base_3)
      },
      icon = '⊞ ',
      right_sep = ' '
    }

    local git_diff_changed = {
      provider = 'git_diff_changed',
      hl = {
        fg = hsl_to_hex(colours.base_1)
      },
      icon = '⊡ ',
      right_sep = ' '
    }

    local git_diff_removed = {
      provider = 'git_diff_removed',
      hl = {
        fg = hsl_to_hex(colours.base_0)
      },
      icon = '⊟ ',
      right_sep = ' '
    }

    local position = {
      provider = 'position',
      hl = {
        fg = hsl_to_hex(colours.base_4)
      },
      left_sep = '',
      right_sep = ' '
    }

    local scroll_bar = {
      provider = 'scroll_bar',
      hl = {
        fg = hsl_to_hex(colours.base_5)
      },
      opts = {
        reverse = true
      },
    }

    require('feline').setup({
      default_bg = hsl_to_hex(colours.bg_1),
      default_fg = hsl_to_hex(colours.fg),
      components = {
        active = {
          -- Left
          {
            left_banner,
            vi_mode,
            file_info,
            lsp_client_names,
            lsp_diag_errors,
            lsp_diag_warnings,
            lsp_diag_hints,
            lsp_diag_info,
          },
          -- Mid
          {},
          -- Right
          {
            git_branch,
            git_diff_added,
            git_diff_changed,
            git_diff_removed,
            position,
            scroll_bar,
          }
        },
        inactive = {
          -- Left
          {
            left_banner,
            file_info,
          },
          -- Mid
          {},
          -- Right
          {
            git_branch,
            position,
            scroll_bar,
          },
        }
      },
      vi_mode_colors = {
        NORMAL = hsl_to_hex(colours.base_3),
        INSERT = hsl_to_hex(colours.base_4),
        VISUAL = hsl_to_hex(colours.base_7),
        OP = hsl_to_hex(colours.base_0),
        BLOCK = hsl_to_hex(colours.base_2),
        REPLACE = hsl_to_hex(colours.base_6),
        ['V-REPLACE'] = hsl_to_hex(colours.base_6),
        ENTER = hsl_to_hex(colours.base_4),
        MORE = hsl_to_hex(colours.base_4),
        SELECT = hsl_to_hex(colours.base_2),
        COMMAND = hsl_to_hex(colours.base_1),
        SHELL = hsl_to_hex(colours.base_3),
        TERM = hsl_to_hex(colours.base_3),
        NONE = hsl_to_hex(colours.base_2)
      }
    })
  end
}
