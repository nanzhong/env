require('packer').use {
  'neovim/nvim-lspconfig',
  requires = { 'hrsh7th/cmp-nvim-lsp' },
  config = function ()
    vim.fn.sign_define('DiagnosticSignError', { texthl = 'DiagnosticSignError', text = '‼⚑' })
    vim.fn.sign_define('DiagnosticSignWarn', { texthl = 'DiagnosticSignWarn', text = '!'  })
    vim.fn.sign_define('DiagnosticSignHint', { texthl = 'DiagnosticSignHint', text = '?' })
    vim.fn.sign_define('DiagnosticSignInfo', { texthl = 'DiagnosticSignInfo', text = '*' })

    local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

    vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, {
      border = 'rounded',
    })

    vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, {
      border = 'rounded',
    })

    require('lspconfig')['bashls'].setup({
      capabilities = capabilities,
    })
    require('lspconfig')['cssls'].setup({
      capabilities = capabilities,
    })
    require('lspconfig')['gopls'].setup({
      capabilities = capabilities,
    })
    require('lspconfig')['html'].setup({
      capabilities = capabilities,
    })
    require('lspconfig')['jsonls'].setup({
      capabilities = capabilities,
    })
    require('lspconfig')['rust_analyzer'].setup({
      capabilities = capabilities,
    })
    require('lspconfig')['sqls'].setup({
      capabilities = capabilities,
    })
    require('lspconfig')['sumneko_lua'].setup({
      capabilities = capabilities,
      settings = {
        Lua = {
          runtime = {
            version = 'LuaJIT',
          },
          diagnostics = {
            globals = {'vim'},
          },
          workspace = {
            library = vim.api.nvim_get_runtime_file("", true),
          },
          telemetry = {
            enable = false,
          },
        },
      },
    })
    require('lspconfig')['tsserver'].setup({
      capabilities = capabilities,
    })
  end
}
