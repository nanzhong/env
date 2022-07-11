require('packer').use {
  'feline-nvim/feline.nvim',
  requires = { 'rktjmp/lush.nvim' },
  config = function ()
    local hsl_to_hex = require('lush.hsl.convert').hsl_to_hex
    local colours = require('lush_theme.nan.colours')
    local icons = {
      errs = ' ',
      warns = ' ',
      infos = ' ',
      hints = ' ',

      lsp = ' ',
      git = ''
    }

    local vi_mode_text = {
      n = "NORMAL",
      i = "INSERT",
      v = "VISUAL",
      [''] = "V-BLOCK",
      V = "V-LINE",
      c = "COMMAND",
      no = "UNKNOWN",
      s = "UNKNOWN",
      S = "UNKNOWN",
      ic = "UNKNOWN",
      R = "REPLACE",
      Rv = "UNKNOWN",
      cv = "UNKWON",
      ce = "UNKNOWN",
      r = "REPLACE",
      rm = "UNKNOWN",
      t = "INSERT"
    }

    local function lsp_diagnostics_info()
      return {
        errs = lsp.get_diagnostics_count('Error'),
        warns = lsp.get_diagnostics_count('Warning'),
        infos = lsp.get_diagnostics_count('Information'),
        hints = lsp.get_diagnostics_count('Hint')
      }
    end

    local function diag_enabled(f, s)
      return function()
        local diag = f()[s]
        return diag and diag ~= 0
      end
    end

    local function diag_provider(f, s)
      local icon = icons[s]
      return function()
        local diag = f()[s]
        return icon .. diag
      end
    end

    local comps = {
      vi_mode = {
        left = {
          provider = function ()
            local current_text = ' '..vi_mode_text[vim.fn.mode()]..' '
            return current_text
          end,
          hl = function ()
            local val = {
              name = vi_mode_utils.get_mode_highlight_name(),
              fg = colorus.bg_1,
              bg = vi_mode_utils.get_mode_color(),
              style = 'bold'
            }
            return val
          end
        },
        right = {
          provider = '▊',
          hl = function()
            local val = {
              name = vi_mode_utils.get_mode_highlight_name(),
              fg = vi_mode_utils.get_mode_color()
            }
            return val
          end,
        }
      },
      file = {
        info = {
          provider = 'file_info',
          hl = {
            fg = colours.base_5,
            style = 'bold'
          }
        },
        encoding = {
          provider = 'file_encoding',
          hl = {
            fg = colours.base_6,
            style = 'bold'
          }
        },
        type = {
          provider = 'file_type'
        },
      },
      line_percentage = {
        provider = 'line_percentage',
        hl = {
          style = 'bold'
        }
      },
      scroll_bar = {
        provider = 'scroll_bar',
        hl = {
          fg = colours.base_5,
          style = 'bold'
        }
      },
      diagnos = {
        err = {
          provider = diag_provider(lsp_diagnostics_info, 'errs'),
          enabled = diag_enabled(lsp_diagnostics_info, 'errs'),
          hl = {
            fg = colours.base_0
          }
        },
        warn = {
          provider = diag_provider(lsp_diagnostics_info, 'warns'),
          enabled = diag_enabled(lsp_diagnostics_info, 'warns'),
          hl = {
            fg = colours.base_2
          }
        },
        info = {
          provider = diag_provider(lsp_diagnostics_info, 'infos'),
          enabled = diag_enabled(lsp_diagnostics_info, 'infos'),
          hl = {
            fg = colours.base_5
          }
        },
        hint = {
          provider = diag_provider(lsp_diagnostics_info, 'hints'),
          enabled = diag_enabled(lsp_diagnostics_info, 'hints'),
          hl = {
            fg = colours.base_4
          }
        },
      },
      lsp = {
        name = {
          provider = 'lsp_client_names',
          icon = icons.lsp,
          hl = {
            fg = colours.base_2
          }
        }
      },
      git = {
        branch = {
          provider = 'git_branch',
          icon = icons.git,
          hl = {
            fg = colours.base_6,
            style = 'bold'
          },
        },
        add = {
          provider = 'git_diff_added',
          hl = {
            fg = colours.base_3
          }
        },
        change = {
          provider = 'git_diff_changed',
          hl = {
            fg = colours.base_1
          }
        },
        remove = {
          provider = 'git_diff_removed',
          hl = {
            fg = colours.base_0
          }
        }
      }
    }

    require('feline').setup({
      default_bg = hsl_to_hex(colours.bg_1),
      default_fg = hsl_to_hex(colours.fg),
      components = {
        active = {
          {
            provider = 'vi_mode',
            hl = function()
              return {
                name = require('feline.providers.vi_mode').get_mode_highlight_name(),
                fg = require('feline.providers.vi_mode').get_mode_color(),
                style = 'bold'
              }
            end,
            right_sep = ' '
          },
        },
        inactive = {}
      },
      vi_mode_colors = {
        NORMAL = hsl_to_hex(colours.base_4),
        INSERT = hsl_to_hex(colours.base_0),
        VISUAL = hsl_to_hex(colours.base_7),
        OP = hsl_to_hex(colours.base_4),
        BLOCK = hsl_to_hex(colours.base_5),
        REPLACE = hsl_to_hex(colours.base_6),
        ['V-REPLACE'] = hsl_to_hex(colours.base_6),
        ENTER = hsl_to_hex(colours.base_4),
        MORE = hsl_to_hex(colours.base_4),
        SELECT = hsl_to_hex(colours.base_1),
        COMMAND = hsl_to_hex(colours.base_3),
        SHELL = hsl_to_hex(colours.base_3),
        TERM = hsl_to_hex(colours.base_3),
        NONE = hsl_to_hex(colours.base_2)
      }
    })
  end
}
