local lush = require('lush')

vim.opt.background = 'dark'
vim.g.colors_name = 'nan'
package.loaded['lush_theme.nan'] = nil
lush.apply(require('lush_theme.nan'))
