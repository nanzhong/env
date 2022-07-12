require('packer').use {
  'neovim/nvim-lspconfig',
  config = function ()
    require('lspconfig')['bashls'].setup({})
    require('lspconfig')['cssls'].setup({})
    require('lspconfig')['gopls'].setup({})
    require('lspconfig')['html'].setup({})
    require('lspconfig')['jsonls'].setup({})
    require('lspconfig')['rust_analyzer'].setup({})
    require('lspconfig')['sqls'].setup({})
    require('lspconfig')['sumneko_lua'].setup({
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
    require('lspconfig')['tsserver'].setup({})
  end
}
