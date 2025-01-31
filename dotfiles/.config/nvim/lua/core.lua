-- general
vim.opt.autochdir = true
vim.opt.mouse = "a"
vim.opt.swapfile = false
vim.opt.timeoutlen = 500
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- search
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- visual
vim.opt.cc = vim.opt.cc + { 80 }
vim.opt.number = true
vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "_" }
vim.opt.termguicolors = true
vim.opt.cursorline = true

-- tab settings
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
vim.opt.expandtab = true

vim.keymap.set("n", "<CR>", "<Cmd>nohlsearch<CR>", { desc = "Clear search highlight", noremap = true, silent = true })
