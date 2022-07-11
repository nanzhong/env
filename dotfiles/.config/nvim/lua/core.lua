-- general
vim.opt.autochdir = true
vim.opt.mouse = 'a'
vim.opt.swapfile = false
vim.g.mapleader = ' '

-- visual
vim.opt.cc = vim.opt.cc + { 80  }
vim.opt.number = true
vim.opt.listchars = { tab = '» ', trail = '·', nbsp = '_' }
vim.opt.termguicolors = true

-- tab settings
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
vim.opt.expandtab = true
