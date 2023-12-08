package.loaded['lush_theme.nan.colours'] = nil
local colours = require('lush_theme.nan.colours')

---@diagnostic disable: undefined-global
local theme = require('lush')(function(injected_functions)
  local sym = injected_functions.sym
  return {
    ColorColumn         { bg = colours.bg_1 }, -- Columns set with 'colorcolumn'
    Conceal             { fg = colours.fg_1 }, -- Placeholder characters substituted for concealed text (see 'conceallevel')
    Cursor              { bg = colours.fg_3, fg = colours.fg }, -- Character under the cursor
    -- CurSearch           { }, -- Highlighting a search pattern under the cursor (see 'hlsearch')
    -- CursorIM            { }, -- Like Cursor, but used when in IME mode |CursorIM|
    -- lCursor             { }, -- Character under the cursor when |language-mapping| is used (see 'guicursor')
    CursorColumn        { bg = colours.bg_1 }, -- Screen-column at the cursor, when 'cursorcolumn' is set.
    CursorLine          { bg = colours.bg_1 }, -- Screen-line at the cursor, when 'cursorline' is set. Low-priority if foreground (ctermfg OR guifg) is not set.
    Directory           { fg = colours.base_3 }, -- Directory names (and other special names in listings)
    DiffAdd             { fg = colours.base_3 }, -- Diff mode: Added line |diff.txt|
    DiffChange          { fg = colours.base_1 }, -- Diff mode: Changed line |diff.txt|
    DiffDelete          { fg = colours.base_0 }, -- Diff mode: Deleted line |diff.txt|
    DiffText            { fg = colours.base_4 }, -- Diff mode: Changed text within a changed line |diff.txt|
    EndOfBuffer         { fg = colours.fg_3 }, -- Filler lines (~) after the end of the buffer. By default, this is highlighted like |hl-NonText|.
    -- TermCursor          { }, -- Cursor in a focused terminal
    -- TermCursorNC        { }, -- Cursor in an unfocused terminal
    ErrorMsg            { bg = colours.base_0, fg = colours.white }, -- Error messages on the command line
    VertSplit           { bg = colours.bg, fg = colours.bg_2 }, -- Column separating vertically split windows
    Folded              { bg = colours.bg_1, fg = colours.fg_1 }, -- Line used for closed folds
    FoldColumn          { bg = colours.bg_1, fg = colours.fg_1 }, -- 'foldcolumn'
    SignColumn          { bg = colours.bg_1, fg = colours.fg_2 }, -- Column where |signs| are displayed
    IncSearch           { bg = colours.bg_3, fg = colours.base_2 }, -- 'incsearch' highlighting; also used for the text replaced with ":s///c"
    Substitute          { bg = colours.bg_3, fg = colours.base_7 }, -- |:substitute| replacement text highlighting
    LineNr              { bg = colours.bg_1, fg = colours.bg_3 }, -- Line number for ":number" and ":#" commands, and when 'number' or 'relativenumber' option is set.
    -- LineNrAbove         { }, -- Line number for when the 'relativenumber' option is set, above the cursor line
    -- LineNrBelow         { }, -- Line number for when the 'relativenumber' option is set, below the cursor line
    CursorLineNr        { bg = colours.bg_1, fg = colours.fg_2 }, -- Like LineNr when 'cursorline' or 'relativenumber' is set for the cursor line.
    -- CursorLineFold      { }, -- Like FoldColumn when 'cursorline' is set for the cursor line
    -- CursorLineSign      { }, -- Like SignColumn when 'cursorline' is set for the cursor line
    MatchParen          { fg = colours.base_7, gui = "bold" }, -- Character under the cursor or just before it, if it is a paired bracket, and its match. |pi_paren.txt|
    ModeMsg             { fg = colours.fg, gui = "bold" }, -- 'showmode' message (e.g., "-- INSERT -- ")
    MsgArea             { fg = colours.fg }, -- Area for messages and cmdline
    MsgSeparator        { fg = colours.fg_1 }, -- Separator for scrolled messages, `msgsep` flag of 'display'
    MoreMsg             { fg = colours.base_3 }, -- |more-prompt|
    NonText             { fg = colours.fg_3 }, -- '@' at the end of the window, characters from 'showbreak' and other characters that do not really exist in the text (e.g., ">" displayed when a double-wide character doesn't fit at the end of the line). See also |hl-EndOfBuffer|.
    Normal              { bg = colours.bg, fg = colours.fg }, -- Normal text
    NormalFloat         { Normal }, -- Normal text in floating windows.
    NormalNC            { Normal }, -- normal text in non-current windows
    FloatTitle          { fg = colours.fg, gui = 'bold' },
    FloatBorder         { fg = colours.bg_2 },
    Pmenu               { bg = colours.bg_1 }, -- Popup menu: Normal item.
    -- PmenuKind           { }, -- Popup menu: Normal item "kind"
    -- PmenuKindSel        { }, -- Popup menu: Selected item "kind"
    -- PmenuExtra          { }, -- Popup menu: Normal item "extra text"
    -- PmenuExtraSel       { }, -- Popup menu: Selected item "extra text"
    PmenuSel            { bg = colours.bg_3 }, -- Popup menu: Selected item.
    PmenuSbar           { bg = colours.bg_2 }, -- Popup menu: Scrollbar.
    PmenuThumb          { bg = colours.bg_3 }, -- Popup menu: Thumb of the scrollbar.
    Question            { fg = colours.base_7 }, -- |hit-enter| prompt and yes/no questions
    QuickFixLine        { fg = colours.base_1}, -- Current |quickfix| item in the quickfix window. Combined with |hl-CursorLine| when the cursor is there.
    Search              { bg = colours.base_2 }, -- Last search pattern highlighting (see 'hlsearch'). Also used for similar items that need to stand out.
    SpecialKey          { fg = colours.fg_3 }, -- Unprintable characters: text displayed differently from what it really is. But not 'listchars' whitespace. |hl-Whitespace|
    SpellBad            { fg = colours.base_0, gui = "underline" }, -- Word that is not recognized by the spellchecker. |spell| Combined with the highlighting used otherwise.
    SpellCap            { fg = colours.base_2 }, -- Word that should start with a capital. |spell| Combined with the highlighting used otherwise.
    SpellLocal          { fg = colours.base_2 }, -- Word that is recognized by the spellchecker as one that is used in another region. |spell| Combined with the highlighting used otherwise.
    SpellRare           { fg = colours.base_2 }, -- Word that is recognized by the spellchecker as one that is hardly ever used. |spell| Combined with the highlighting used otherwise.
    StatusLine          { bg = colours.bg_1, fg = colours.fg }, -- Status line of current window
    StatusLineNC        { bg = colours.bg_1, fg = colours.fg_1 }, -- Status lines of not-current windows. Note: If this is equal to "StatusLine" Vim will use "^^^" in the status line of the current window.
    TabLine             { bg = colours.bg_1 }, -- Tab pages line, not active tab page label
    TabLineFill         { bg = colours.bg }, -- Tab pages line, where there are no labels
    TabLineSel          { bg = colours.bg_2 }, -- Tab pages line, active tab page label
    Title               { fg = colours.base_3, gui = "bold" }, -- Titles for output from ":set all", ":autocmd" etc.
    Visual              { bg = colours.bg_3, fg = colours.base_1 }, -- Visual mode selection
    VisualNOS           { bg = colours.bg_3, fg = colours.base_0,  gui = "bold" }, -- Visual mode selection when vim is "Not Owning the Selection".
    WarningMsg          { fg = colours.base_2 }, -- Warning messages
    Whitespace          { fg = colours.fg_3 }, -- "nbsp", "space", "tab" and "trail" in 'listchars'
    Winseparator        { bg = colours.bg, fg = colours.bg_2 }, -- Separator between window splits. Inherts from |hl-VertSplit| by default, which it will replace eventually.
    WildMenu            { bg = colours.bg_2 }, -- Current match in 'wildmenu' completion
    -- WinBar              { }, -- Window bar of current window
    -- WinBarNC            { }, -- Window bar of not-current windows

    -- Common vim syntax groups used for all kinds of code and markup.
    -- Commented-out groups should chain up to their preferred (*) group
    -- by default.
    --
    -- See :h group-name
    --
    -- Uncomment and edit if you want more specific syntax highlighting.

    Comment        { fg = colours.fg_2 }, -- Any comment

    Constant       { fg = colours.base_1 }, -- (*) Any constant
    String         { fg = colours.fg_1 }, --   A string constant: "this is a string"
    Character      { fg = colours.fg_1 }, --   A character constant: 'c', '\n'
    Number         { fg = colours.base_4 }, --   A number constant: 234, 0xff
    Boolean        { fg = colours.base_3 }, --   A boolean constant: TRUE, false
    Float          { fg = colours.base_4}, --   A floating point constant: 2.3e10

    Identifier     { fg = colours.base_4 }, -- (*) Any variable name
    Function       { fg = colours.base_6 }, --   Function name (also: methods for classes)

    Statement      { fg = colours.base_7 }, -- (*) Any statement
    -- Conditional    { }, --   if, then, else, endif, switch, etc.
    -- Repeat         { }, --   for, do, while, etc.
    -- Label          { }, --   case, default, etc.
    -- Operator       { }, --   "sizeof", "+", "*", etc.
    -- Keyword        { }, --   any other keyword
    -- Exception      { }, --   try, catch, throw

    PreProc        { fg = colours.base_6 }, -- (*) Generic Preprocessor
    -- Include        { }, --   Preprocessor #include
    -- Define         { }, --   Preprocessor #define
    -- Macro          { }, --   Same as Define
    -- PreCondit      { }, --   Preprocessor #if, #else, #endif, etc.

    Type           { fg = colours.base_3 }, -- (*) int, long, char, etc.
    -- StorageClass   { }, --   static, register, volatile, etc.
    -- Structure      { }, --   struct, union, enum, etc.
    -- Typedef        { }, --   A typedef

    Special        { fg = colours.base_0 }, -- (*) Any special symbol
    -- SpecialChar    { }, --   Special character in a constant
    -- Tag            { }, --   You can use CTRL-] on this
    -- Delimiter      { }, --   Character that needs attention
    -- SpecialComment { }, --   Special things inside a comment (e.g. '\n')
    -- Debug          { }, --   Debugging statements

    Underlined     { gui = "underline" }, -- Text that stands out, HTML links
    -- Ignore         { }, -- Left blank, hidden |hl-Ignore| (NOTE: May be invisible here in template)
    Error          { bg = colours.base_0, fg = colours.fg_3 }, -- Any erroneous construct
    Todo           { bg = colours.base_2, fg = colours.fg_3 }, -- Anything that needs extra attention; mostly the keywords TODO FIXME and XXX

    -- These groups are for the native LSP client and diagnostic system. Some
    -- other LSP clients may use these groups, or use their own. Consult your
    -- LSP client's documentation.

    -- See :h lsp-highlight, some groups may not be listed, submit a PR fix to lush-template!
    --
    -- LspReferenceText            { } , -- Used for highlighting "text" references
    -- LspReferenceRead            { } , -- Used for highlighting "read" references
    -- LspReferenceWrite           { } , -- Used for highlighting "write" references
    -- LspCodeLens                 { } , -- Used to color the virtual text of the codelens. See |nvim_buf_set_extmark()|.
    -- LspCodeLensSeparator        { } , -- Used to color the seperator between two or more code lens.
    -- LspSignatureActiveParameter { } , -- Used to highlight the active parameter in the signature help. See |vim.lsp.handlers.signature_help()|.

    -- See :h diagnostic-highlights, some groups may not be listed, submit a PR fix to lush-template!
    --
    DiagnosticError            { fg = colours.base_0 } , -- Used as the base highlight group. Other Diagnostic highlights link to this by default (except Underline)
    DiagnosticWarn             { fg = colours.base_2 } , -- Used as the base highlight group. Other Diagnostic highlights link to this by default (except Underline)
    DiagnosticInfo             { fg = colours.fg_2 } , -- Used as the base highlight group. Other Diagnostic highlights link to this by default (except Underline)
    DiagnosticHint             { fg = colours.fg_3 } , -- Used as the base highlight group. Other Diagnostic highlights link to this by default (except Underline)
    DiagnosticOk               { fg = colours.base_3 } , -- Used as the base highlight group. Other Diagnostic highlights link to this by default (except Underline)
    -- DiagnosticVirtualTextError { } , -- Used for "Error" diagnostic virtual text.
    -- DiagnosticVirtualTextWarn  { } , -- Used for "Warn" diagnostic virtual text.
    -- DiagnosticVirtualTextInfo  { } , -- Used for "Info" diagnostic virtual text.
    -- DiagnosticVirtualTextHint  { } , -- Used for "Hint" diagnostic virtual text.
    -- DiagnosticVirtualTextOk    { } , -- Used for "Ok" diagnostic virtual text.
    DiagnosticUnderlineError   { fg = colours.base_0, gui = 'underline' } , -- Used to underline "Error" diagnostics.
    DiagnosticUnderlineWarn    { fg = colours.base_2, gui = 'underline' } , -- Used to underline "Warn" diagnostics.
    DiagnosticUnderlineInfo    { fg = colours.fg_2, gui = 'underline' } , -- Used to underline "Info" diagnostics.
    DiagnosticUnderlineHint    { fg = colours.fg_3, gui = 'underline' } , -- Used to underline "Hint" diagnostics.
    DiagnosticUnderlineOk      { fg = colours.base_3, gui = 'underline' } , -- Used to underline "Ok" diagnostics.
    DiagnosticFloatingError    { fg = colours.base_0 } , -- Used to color "Error" diagnostic messages in diagnostics float. See |vim.diagnostic.open_float()|
    DiagnosticFloatingWarn     { fg = colours.base_2 } , -- Used to color "Warn" diagnostic messages in diagnostics float.
    DiagnosticFloatingInfo     { fg = colours.fg_2 } , -- Used to color "Info" diagnostic messages in diagnostics float.
    DiagnosticFloatingHint     { fg = colours.fg_3 } , -- Used to color "Hint" diagnostic messages in diagnostics float.
    DiagnosticFloatingOk       { fg = colours.base_3 } , -- Used to color "Ok" diagnostic messages in diagnostics float.
    DiagnosticSignError        { bg = colours.bg_1, fg = colours.base_0 } , -- Used for "Error" signs in sign column.
    DiagnosticSignWarn         { bg = colours.bg_1, fg = colours.base_2 } , -- Used for "Warn" signs in sign column.
    DiagnosticSignInfo         { bg = colours.bg_1, fg = colours.fg_2 } , -- Used for "Info" signs in sign column.
    DiagnosticSignHint         { bg = colours.bg_1, fg = colours.fg_3 } , -- Used for "Hint" signs in sign column.
    DiagnosticSignOk           { bg = colours.bg_1, fg = colours.base_3 } , -- Used for "Ok" signs in sign column.

    -- Tree-Sitter syntax groups.
    --
    -- See :h treesitter-highlight-groups, some groups may not be listed,
    -- submit a PR fix to lush-template!
    --
    -- Tree-Sitter groups are defined with an "@" symbol, which must be
    -- specially handled to be valid lua code, we do this via the special
    -- sym function. The following are all valid ways to call the sym function,
    -- for more details see https://www.lua.org/pil/5.html
    --
    -- sym("@text.literal")
    -- sym('@text.literal')
    -- sym"@text.literal"
    -- sym'@text.literal'
    --
    -- For more information see https://github.com/rktjmp/lush.nvim/issues/109

    -- sym"@text.literal"      { }, -- Comment
    -- sym"@text.reference"    { }, -- Identifier
    -- sym"@text.title"        { }, -- Title
    -- sym"@text.uri"          { }, -- Underlined
    -- sym"@text.underline"    { }, -- Underlined
    -- sym"@text.todo"         { }, -- Todo
    -- sym"@comment"           { }, -- Comment
    -- sym"@punctuation"       { }, -- Delimiter
    -- sym"@constant"          { }, -- Constant
    -- sym"@constant.builtin"  { }, -- Special
    -- sym"@constant.macro"    { }, -- Define
    -- sym"@define"            { }, -- Define
    -- sym"@macro"             { }, -- Macro
    -- sym"@string"            { }, -- String
    -- sym"@string.escape"     { }, -- SpecialChar
    -- sym"@string.special"    { }, -- SpecialChar
    -- sym"@character"         { }, -- Character
    -- sym"@character.special" { }, -- SpecialChar
    -- sym"@number"            { }, -- Number
    -- sym"@boolean"           { }, -- Boolean
    -- sym"@float"             { }, -- Float
    -- sym"@function"          { }, -- Function
    -- sym"@function.builtin"  { }, -- Special
    -- sym"@function.macro"    { }, -- Macro
    -- sym"@parameter"         { }, -- Identifier
    -- sym"@method"            { }, -- Function
    -- sym"@field"             { }, -- Identifier
    -- sym"@property"          { }, -- Identifier
    -- sym"@constructor"       { }, -- Special
    -- sym"@conditional"       { }, -- Conditional
    -- sym"@repeat"            { }, -- Repeat
    -- sym"@label"             { }, -- Label
    -- sym"@operator"          { }, -- Operator
    -- sym"@keyword"           { }, -- Keyword
    -- sym"@exception"         { }, -- Exception
    -- sym"@variable"          { }, -- Identifier
    -- sym"@type"              { }, -- Type
    -- sym"@type.definition"   { }, -- Typedef
    -- sym"@storageclass"      { }, -- StorageClass
    -- sym"@structure"         { }, -- Structure
    -- sym"@namespace"         { }, -- Identifier
    -- sym"@include"           { }, -- Include
    -- sym"@preproc"           { }, -- PreProc
    -- sym"@debug"             { }, -- Debug
    -- sym"@tag"               { }, -- Tag

    -- Cmp
    CmpNormal    { Normal },
    CmpSelection { fg = colours.fg, bg = colours.bg_3, gui = 'bold' },
    CmpBorder    { FloatBorder },

    -- GitSigns
    GitSignsAdd    { bg = colours.bg_1, fg = colours.base_3 },
    GitSignsChange { bg = colours.bg_1, fg = colours.base_1 },
    GitSignsDelete { bg = colours.bg_1, fg = colours.base_0 },

    -- Telescope.nvim
    TelescopeNormal         { Normal },
    TelescopeTitle          { fg = colours.fg, gui = 'bold' },
    TelescopeBorder         { fg = colours.bg_2 },
    TelescopeSelectionCaret { fg = colours.base_3 },
		TelescopeSelection      { fg = colours.fg, bg = colours.bg_3, gui = 'bold' },
		TelescopeMatching       { fg = colours.base_7 },

    -- LightBulb
    LightBulbSign         { bg = colours.bg_1, fg = colours.fg } , -- Used for "Hint" signs in sign column.

    -- Indent-Blankline.nvim
    IndentGuide { bg = colours.bg, fg = colours.bg_1 },
  }
end)

return theme
