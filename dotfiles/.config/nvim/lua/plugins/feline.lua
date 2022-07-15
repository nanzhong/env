require('packer').use {
  'feline-nvim/feline.nvim',
  requires = { 'rktjmp/lush.nvim', 'kyazdani42/nvim-web-devicons' },
  config = function()
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
          fg = hsl_to_hex(colours.bg_1),
          style = 'bold'
        }
      end,
      icon = '',
      left_sep = {
        str = ' ',
        hl = function()
          return {
            bg = require('feline.providers.vi_mode').get_mode_color(),
          }
        end
      },
      right_sep = {
        str = ' ',
        hl = function()
          return {
            bg = require('feline.providers.vi_mode').get_mode_color(),
          }
        end
      }
    }

    local file_info = {
      provider = {
        name = 'file_info',
        opts = {
          file_modified_icon = '●',
          file_readonly_icon = ' ',
          type = 'unique'
        },
      },
      hl = {
        fg = hsl_to_hex(colours.white),
        style = 'bold',
      },
      left_sep = ' '
    }

    local lsp_client_names = {
      provider = 'lsp_client_names',
      hl = {
        fg = hsl_to_hex(colours.base_1),
      },
      right_sep = ' '
    }

    local lsp_diag_errors = {
      provider = 'diagnostic_errors',
      hl = {
        bg = hsl_to_hex(colours.base_0),
        fg = hsl_to_hex(colours.bg_1),
        style = 'bold'
      },
      icon = '‼ ',
      left_sep = {
        str = ' ',
        hl = {
          bg = hsl_to_hex(colours.base_0),
        }
      },
      right_sep = {
        {
          str = ' ',
          hl = {
            bg = hsl_to_hex(colours.base_0),
          },
        },
        ' ',
      },
    }

    local lsp_diag_warnings = {
      provider = 'diagnostic_warnings',
      hl = {
        bg = hsl_to_hex(colours.base_2),
        fg = hsl_to_hex(colours.bg_1),
        style = 'bold'
      },
      icon = '! ',
      left_sep = {
        str = ' ',
        hl = {
          bg = hsl_to_hex(colours.base_2),
        }
      },
      right_sep = {
        {
          str = ' ',
          hl = {
            bg = hsl_to_hex(colours.base_2),
          },
        },
        ' ',
      }
    }

    local lsp_diag_hints = {
      provider = 'diagnostic_hints',
      hl = {
        bg = hsl_to_hex(colours.base_4),
        fg = hsl_to_hex(colours.bg_1),
        style = 'bold'
      },
      icon = '? ',
      left_sep = {
        str = ' ',
        hl = {
          bg = hsl_to_hex(colours.base_4),
        }
      },
      right_sep = {
        {
          str = ' ',
          hl = {
            bg = hsl_to_hex(colours.base_4),
          },
        },
        ' ',
      }
    }

    local lsp_diag_info = {
      provider = 'diagnostic_info',
      hl = {
        bg = hsl_to_hex(colours.fg),
        fg = hsl_to_hex(colours.bg_1),
        style = 'bold'
      },
      icon = '* ',
      left_sep = {
        str = ' ',
        hl = {
          bg = hsl_to_hex(colours.fg),
        }
      },
      right_sep = {
        {
          str = ' ',
          hl = {
            bg = hsl_to_hex(colours.fg),
          },
        },
        ' ',
      },
    }

    local git_branch = {
      provider = 'git_branch',
      hl = {
        fg = hsl_to_hex(colours.base_7)
      },
      icon = ' ',
      left_sep = ' '
    }

    local git_diff_added = {
      provider = 'git_diff_added',
      hl = {
        fg = hsl_to_hex(colours.base_3)
      },
      icon = '⊞ ',
      left_sep = ' '
    }

    local git_diff_changed = {
      provider = 'git_diff_changed',
      hl = {
        fg = hsl_to_hex(colours.base_1)
      },
      icon = '⊡ ',
      left_sep = ' '
    }

    local git_diff_removed = {
      provider = 'git_diff_removed',
      hl = {
        fg = hsl_to_hex(colours.base_0)
      },
      icon = '⊟ ',
      left_sep = ' '
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
        fg = hsl_to_hex(colours.base_4)
      },
      opts = {
        reverse = true
      },
    }

    require('feline').setup({
      theme = {
        fg        = hsl_to_hex(colours.fg),
        bg        = hsl_to_hex(colours.bg_1),
        white     = hsl_to_hex(colours.white),
        black     = hsl_to_hex(colours.black),
        red       = hsl_to_hex(colours.base_0),
        orange    = hsl_to_hex(colours.base_1),
        yellow    = hsl_to_hex(colours.base_2),
        green     = hsl_to_hex(colours.base_3),
        cyan      = hsl_to_hex(colours.base_4),
        skyblue   = hsl_to_hex(colours.base_5),
        oceanblue = hsl_to_hex(colours.base_5),
        violet    = hsl_to_hex(colours.base_6),
        magenta   = hsl_to_hex(colours.base_7),
      },
      components = {
        active = {
          -- Left
          {
            left_banner,
            vi_mode,
            file_info,
            git_branch,
            git_diff_added,
            git_diff_changed,
            git_diff_removed,
          },
          -- Mid
          {},
          -- Right
          {
            lsp_client_names,
            lsp_diag_errors,
            lsp_diag_warnings,
            lsp_diag_hints,
            lsp_diag_info,
            position,
            scroll_bar,
          }
        },
        inactive = {
          -- Left
          {
            left_banner,
            file_info,
            git_branch,
          },
          -- Mid
          {},
          -- Right
          {
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
