require('packer').use {
  'glepnir/lspsaga.nvim',
  config = function ()
    require('lspsaga').init_lsp_saga()

    vim.keymap.set("n", "<C-f>", function()
      require('lspsaga.action').smart_scroll_with_saga(1)
    end, { silent = true })
    vim.keymap.set("n", "<C-b>", function()
      require('lspsaga.action').smart_scroll_with_saga(-1)
    end, { silent = true })

    vim.keymap.set("n", "<leader>lgd", require("lspsaga.finder").lsp_finder, { silent = true, noremap = true })
    vim.keymap.set("n", "<leader>lca", require('lspsaga.codeaction').code_action, { silent = true, noremap = true })
    vim.keymap.set("v", "<leader>lca", function ()
      vim.fn.feedkeys(vim.api.nvim_replace_termcodes("<C-U>", true, false, true))
      require('lspsaga.codeaction').range_code_action()
    end, { silent = true, noremap =true })
    vim.keymap.set("n", "<leader>ldo", require("lspsaga.hover").render_hover_doc, { silent = true })
    vim.keymap.set("n", "<leader>ls", require("lspsaga.signaturehelp").signature_help, { silent = true, noremap = true})
    vim.keymap.set("n", "<leader>lr", require("lspsaga.rename").lsp_rename, { silent = true, noremap = true })
    vim.keymap.set("n", "<leader>ldf", require("lspsaga.definition").preview_definition, { silent = true, noremap = true })
    vim.keymap.set("n", "<leader>li", require("lspsaga.diagnostic").show_line_diagnostics, { silent = true, noremap = true })
  end
}
