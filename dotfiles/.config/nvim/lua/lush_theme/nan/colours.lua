local hsl = require('lush').hsl
local colours = {
  black = hsl(0, 0, 0),      -- #000000
  white = hsl(0, 0, 100),    -- #ffffff

  bg = hsl(205, 15, 10),     -- #161a1d
  bg_1 = hsl(205, 15, 14),   -- #1e2529
  bg_2 = hsl(205, 15, 18),   -- #272f35
  bg_3 = hsl(205, 15, 22),   -- #303a41

  fg = hsl(205, 0, 90),      -- #e2e6e9
  fg_1 = hsl(205, 0, 70),    -- #a7b4be
  fg_2 = hsl(205, 0, 50),    -- #6c8393
  fg_3 = hsl(205, 0, 30),    -- #414e58

  base_0 = hsl(5, 80, 65),   -- #ed6a5e / #f66355
  base_1 = hsl(25, 80, 65),  -- #ed8e5e / #f68b55
  base_2 = hsl(50, 80, 65),  -- #edd55e / #f6db55
  base_3 = hsl(85, 80, 65),  -- #b2ed5e / #b3f655
  base_4 = hsl(200, 80, 65), -- #5ebeed / #55c1f6
  base_5 = hsl(230, 80, 65), -- #5e76ed / #5570f6
  base_6 = hsl(260, 80, 65), -- #8e5eed / #8b55f6
  base_7 = hsl(330, 80, 65), -- #ed5ea6 /  #f655a6
}

colours.ansi = {
  red = colours.base_0,
  green = colours.base_3,
  yellow = colours.base_2,
  blue = colours.base_5,
  magenta = colours.base_7,
  cyan = colours.base_4,
  bright_red = colours.base_0.saturate(10),
  bright_green = colours.base_3.saturate(10),
  bright_yellow = colours.base_2.saturate(10),
  bright_blue = colours.base_5.saturate(10),
  bright_magenta = colours.base_7.saturate(10),
  bright_cyan = colours.base_4.saturate(10),
}

return colours
