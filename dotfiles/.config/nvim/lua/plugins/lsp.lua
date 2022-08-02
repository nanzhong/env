local packer = require('packer')
packer.use {
  'williamboman/mason.nvim',
  after = 'nvim-lspconfig',
  config = function ()
    require('mason').setup()
  end
}
packer.use {
  'williamboman/mason-lspconfig.nvim',
  after = { 'nvim-lspconfig', 'mason.nvim' },
  config = function ()
    require('mason-lspconfig').setup()
  end
}
require('packer').use {
  'neovim/nvim-lspconfig',
  requires = { 'hrsh7th/cmp-nvim-lsp' },
  config = function()
    local signs = { Error = '‼', Warn = '!', Hint = '?', Info = '*' }
    for type, icon in pairs(signs) do
      local hl = 'DiagnosticSign' .. type
      vim.fn.sign_define(hl, { text = icon, texthl = hl })
    end

    vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, {
      border = 'rounded',
    })

    vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, {
      border = 'rounded',
    })

    local function with_desc(opts, desc)
      return vim.tbl_extend('force', opts, { desc = desc })
    end

    local opts = { noremap = true, silent = true }
    vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, with_desc(opts, 'Open diagnostic float'))
    vim.keymap.set('n', '[e', vim.diagnostic.goto_prev, with_desc(opts, 'Goto prev diagnostic'))
    vim.keymap.set('n', ']e', vim.diagnostic.goto_next, with_desc(opts, 'Goto next diagnostic'))
    vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, with_desc(opts, 'Open diagnastic loclist'))

    local on_attach = function(_, bufnr)
      -- Mappings.
      -- See `:help vim.lsp.*` for documentation on any of the below functions
      local bufopts = { noremap = true, silent = true, buffer = bufnr }
      vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, with_desc(bufopts, 'Goto declaration'))
      vim.keymap.set('n', 'gd', vim.lsp.buf.definition, with_desc(bufopts, 'Goto definition'))
      vim.keymap.set('n', 'gdd', vim.lsp.buf.type_definition, with_desc(bufopts, 'Goto type definition'))
      vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, with_desc(bufopts, 'Goto implementation'))
      vim.keymap.set('n', 'gr', vim.lsp.buf.references, with_desc(bufopts, 'Goto references'))
      vim.keymap.set('n', 'K', vim.lsp.buf.hover, with_desc(bufopts, 'Show documentation'))
      vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, with_desc(bufopts, 'Show signature help'))
      vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, with_desc(bufopts, 'Add workspace folder'))
      vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, with_desc(bufopts, 'Remove workspace folder'))
      vim.keymap.set('n', '<leader>wl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
      end, with_desc(bufopts, 'List workspace folders'))
      vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, with_desc(bufopts, 'Rename'))
      vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, with_desc(bufopts, 'Code actions'))
      vim.keymap.set('n', '<leader>f', vim.lsp.buf.formatting, with_desc(bufopts, 'Format'))
    end

    local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

    require('lspconfig')['bashls'].setup {
      capabilities = capabilities,
      on_attach = on_attach,
    }
    require('lspconfig')['cssls'].setup {
      capabilities = capabilities,
      on_attach = on_attach,
    }
    require('lspconfig')['gopls'].setup {
      capabilities = capabilities,
      on_attach = on_attach,
    }
    require('lspconfig')['html'].setup {
      capabilities = capabilities,
      on_attach = on_attach,
    }
    require('lspconfig')['jsonls'].setup {
      capabilities = capabilities,
      on_attach = on_attach,
    }
    require('lspconfig')['rust_analyzer'].setup {
      capabilities = capabilities,
      on_attach = on_attach,
    }
    require('lspconfig')['sqls'].setup {
      capabilities = capabilities,
      on_attach = on_attach,
    }
    require('lspconfig')['sumneko_lua'].setup {
      capabilities = capabilities,
      on_attach = on_attach,
      settings = {
        Lua = {
          runtime = {
            version = 'LuaJIT',
          },
          diagnostics = {
            globals = { 'vim' },
          },
          workspace = {
            library = vim.api.nvim_get_runtime_file('', true),
          },
          telemetry = {
            enable = false,
          },
        },
      },
    }
    require('lspconfig')['tsserver'].setup {
      capabilities = capabilities,
      on_attach = on_attach,
    }
    require('lspconfig')['vuels'].setup {
      capabilities = capabilities,
      on_attach = on_attach,
    }
  end
}
