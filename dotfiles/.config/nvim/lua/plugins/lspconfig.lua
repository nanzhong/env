require('packer').use {
  'neovim/nvim-lspconfig',
  requires = { 'hrsh7th/cmp-nvim-lsp' },
  config = function ()
    local signs = { Error = '‼', Warn = '!', Hint = '?', Info = '*' }
    for type, icon in pairs(signs) do
      local hl = 'DiagnosticSign' .. type
      vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
    end

    vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, {
      border = 'rounded',
    })

    vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, {
      border = 'rounded',
    })

    local opts = { noremap = true, silent = true }
    vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, opts)
    vim.keymap.set('n', '[e', vim.diagnostic.goto_prev, opts)
    vim.keymap.set('n', ']e', vim.diagnostic.goto_next, opts)
    vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, opts)

    local on_attach = function(client, bufnr)
      -- Mappings.
      -- See `:help vim.lsp.*` for documentation on any of the below functions
      local bufopts = { noremap = true, silent = true, buffer = bufnr }
      vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
      vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
      vim.keymap.set('n', 'gdd', vim.lsp.buf.type_definition, bufopts)
      vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
      vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
      vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
      vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
      vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, bufopts)
      vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
      vim.keymap.set('n', '<leader>wl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
      end, bufopts)
      vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, bufopts)
      vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, bufopts)
      vim.keymap.set('n', '<leader>f', vim.lsp.buf.formatting, bufopts)
    end

    local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

    require('lspconfig')['bashls'].setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })
    require('lspconfig')['cssls'].setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })
    require('lspconfig')['gopls'].setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })
    require('lspconfig')['html'].setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })
    require('lspconfig')['jsonls'].setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })
    require('lspconfig')['rust_analyzer'].setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })
    require('lspconfig')['sqls'].setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })
    require('lspconfig')['sumneko_lua'].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      settings = {
        Lua = {
          runtime = {
            version = 'LuaJIT',
          },
          diagnostics = {
            globals = {'vim'},
          },
          workspace = {
            library = vim.api.nvim_get_runtime_file('', true),
          },
          telemetry = {
            enable = false,
          },
        },
      },
    })
    require('lspconfig')['tsserver'].setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })
  end
}
