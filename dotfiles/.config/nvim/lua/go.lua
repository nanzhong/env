vim.api.nvim_create_augroup("AutoFormat", {})
vim.api.nvim_create_autocmd("BufWritePre", {
  group = "AutoFormat",
  pattern = { "*.go" },
  callback = function()
    vim.lsp.buf.format()
  end
})
