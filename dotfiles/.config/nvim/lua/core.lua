-- general
vim.opt.autochdir = true
vim.opt.mouse = 'a'
vim.opt.swapfile = false
vim.g.mapleader = ' '

-- visual
vim.opt.cc = vim.opt.cc + { 80 }
vim.opt.number = true
vim.opt.listchars = { tab = '» ', trail = '·', nbsp = '_' }
vim.opt.termguicolors = true

-- tab settings
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
vim.opt.expandtab = true

-- OSC52
local function get_visual_selection()
  local modeInfo = vim.api.nvim_get_mode()
  local mode = modeInfo.mode

  local cursor = vim.api.nvim_win_get_cursor(0)
  local cline, ccol = cursor[1], cursor[2]
  local vline, vcol = vim.fn.line('v'), vim.fn.col('v')

  local sline, scol
  local eline, ecol
  if cline == vline then
    if ccol <= vcol then
      sline, scol = cline, ccol
      eline, ecol = vline, vcol
      scol = scol + 1
    else
      sline, scol = vline, vcol
      eline, ecol = cline, ccol
      ecol = ecol + 1
    end
  elseif cline < vline then
    sline, scol = cline, ccol
    eline, ecol = vline, vcol
    scol = scol + 1
  else
    sline, scol = vline, vcol
    eline, ecol = cline, ccol
    ecol = ecol + 1
  end

  if mode == 'V' or mode == 'CTRL-V' or mode == '\22' then
    scol = 1
    ecol = nil
  end

  local lines = vim.api.nvim_buf_get_lines(0, sline - 1, eline, 0)
  if #lines == 0 then return end

  local startText, endText
  if #lines == 1 then
    startText = string.sub(lines[1], scol, ecol)
  else
    startText = string.sub(lines[1], scol)
    endText = string.sub(lines[#lines], 1, ecol)
  end

  local selection = { startText }
  if #lines > 2 then
    vim.list_extend(selection, vim.list_slice(lines, 2, #lines - 1))
  end
  table.insert(selection, endText)

  return table.concat(selection, '\n')
end

local function encode_osc52(string)
  local filename = '/tmp/nvim_clipboard.tmp'
  local file, err = io.open(filename, 'w')
  if not file then
    print('Failed to create clipboard tmpfile: ' .. err)
    return
  end

  _, err = file:write(string)
  if err then
    print('Failed to write to clipboard: ' .. err)
    return
  end
  file:close()

  local code = os.execute('echo -ne $(cat < ' .. filename .. " | base64 -w0 | sed 's/\\(.*\\)/\\\\e]52;c;\\0\\\\x07/') > /dev/tty")
  if code ~= 0 then
    print('Failed to write OSC52 sequence: exit ' .. code)
    return
  end
  os.remove(filename)
end

local function copy_osc52()
  local content = get_visual_selection()
  if not content then
    print('Nothing selected...')
    return
  end
  print('Yanking:\n' .. content)
  encode_osc52(content)
end

vim.keymap.set('v', '<leader>y', copy_osc52, { desc = 'Yank seletion using OSC52' })
