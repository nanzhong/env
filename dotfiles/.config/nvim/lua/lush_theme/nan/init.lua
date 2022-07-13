package.loaded['lush_theme.nan.colours'] = nil
local colours = require('lush_theme.nan.colours')

---@diagnostic disable: undefined-global
local theme = require('lush')(function()
  return {
    ColorColumn  { bg = colours.bg_1 }, -- Columns set with 'colorcolumn'
    Conceal      { fg = colours.fg_1 }, -- Placeholder characters substituted for concealed text (see 'conceallevel')
    Cursor       { bg = colours.fg_3, fg = colours.fg }, -- Character under the cursor
    -- lCursor      { }, -- Character under the cursor when |language-mapping| is used (see 'guicursor')
    -- CursorIM     { }, -- Like Cursor, but used when in IME mode |CursorIM|
    CursorColumn { bg = colours.bg_2 }, -- Screen-column at the cursor, when 'cursorcolumn' is set.
    CursorLine   { bg = colours.bg_2 }, -- Screen-line at the cursor, when 'cursorline' is set. Low-priority if foreground (ctermfg OR guifg) is not set.
    Directory    { fg = colours.base_3 }, -- Directory names (and other special names in listings)
    DiffAdd      { fg = colours.base_3 }, -- Diff mode: Added line |diff.txt|
    DiffChange   { fg = colours.base_1 }, -- Diff mode: Changed line |diff.txt|
    DiffDelete   { fg = colours.base_0 }, -- Diff mode: Deleted line |diff.txt|
    DiffText     { fg = colours.base_4 }, -- Diff mode: Changed text within a changed line |diff.txt|
    EndOfBuffer  { fg = colours.fg_3 }, -- Filler lines (~) after the end of the buffer. By default, this is highlighted like |hl-NonText|.
    -- TermCursor   { }, -- Cursor in a focused terminal
    -- TermCursorNC { }, -- Cursor in an unfocused terminal
    ErrorMsg     { bg = colours.base_0, fg = colours.white }, -- Error messages on the command line
    VertSplit    { bg = colours.bg, fg = colours.bg_2 }, -- Column separating vertically split windows
    Folded       { bg = colours.bg_1, fg = colours.fg_1 }, -- Line used for closed folds
    FoldColumn   { bg = colours.bg_1, fg = colours.fg_1 }, -- 'foldcolumn'
    SignColumn   { bg = colours.bg_1, fg = colours.fg_2 }, -- Column where |signs| are displayed
    IncSearch    { bg = colours.base_2 }, -- 'incsearch' highlighting; also used for the text replaced with ":s///c"
    Substitute   { bg = colours.base_7}, -- |:substitute| replacement text highlighting
    LineNr       { bg = colours.bg_1, fg = colours.fg_3 }, -- Line number for ":number" and ":#" commands, and when 'number' or 'relativenumber' option is set.
    CursorLineNr { bg = colours.bg_2, fg = colours.fg_2 }, -- Like LineNr when 'cursorline' or 'relativenumber' is set for the cursor line.
    MatchParen   { fg = colours.base_7, gui = "bold" }, -- Character under the cursor or just before it, if it is a paired bracket, and its match. |pi_paren.txt|
    ModeMsg      { fg = colours.fg, gui = "bold" }, -- 'showmode' message (e.g., "-- INSERT -- ")
    MsgArea      { fg = colours.fg }, -- Area for messages and cmdline
    MsgSeparator { fg = colours.fg_1 }, -- Separator for scrolled messages, `msgsep` flag of 'display'
    MoreMsg      { fg = colours.base_3 }, -- |more-prompt|
    NonText      { fg = colours.fg_3 }, -- '@' at the end of the window, characters from 'showbreak' and other characters that do not really exist in the text (e.g., ">" displayed when a double-wide character doesn't fit at the end of the line). See also |hl-EndOfBuffer|.
    Normal       { bg = colours.bg, fg = colours.fg }, -- Normal text
    NormalFloat  { Normal }, -- Normal text in floating windows.
    NormalNC     { Normal }, -- normal text in non-current windows
    Pmenu        { bg = colours.bg_1 }, -- Popup menu: Normal item.
    PmenuSel     { bg = colours.bg_3 }, -- Popup menu: Selected item.
    PmenuSbar    { bg = colours.bg_2 }, -- Popup menu: Scrollbar.
    PmenuThumb   { bg = colours.bg_3 }, -- Popup menu: Thumb of the scrollbar.
    Question     { fg = colours.base_7 }, -- |hit-enter| prompt and yes/no questions
    QuickFixLine { fg = colours.base_1}, -- Current |quickfix| item in the quickfix window. Combined with |hl-CursorLine| when the cursor is there.
    Search       { bg = colours.base_2 }, -- Last search pattern highlighting (see 'hlsearch'). Also used for similar items that need to stand out.
    SpecialKey   { fg = colours.fg_3 }, -- Unprintable characters: text displayed differently from what it really is. But not 'listchars' whitespace. |hl-Whitespace|
    SpellBad     { fg = colours.base_0, gui = "underline" }, -- Word that is not recognized by the spellchecker. |spell| Combined with the highlighting used otherwise.
    SpellCap     { fg = colours.base_2 }, -- Word that should start with a capital. |spell| Combined with the highlighting used otherwise.
    SpellLocal   { fg = colours.base_2 }, -- Word that is recognized by the spellchecker as one that is used in another region. |spell| Combined with the highlighting used otherwise.
    SpellRare    { fg = colours.base_2 }, -- Word that is recognized by the spellchecker as one that is hardly ever used. |spell| Combined with the highlighting used otherwise.
    StatusLine   { bg = colours.bg_2 }, -- Status line of current window
    StatusLineNC { bg = colours.bg_1 }, -- Status lines of not-current windows. Note: If this is equal to "StatusLine" Vim will use "^^^" in the status line of the current window.
    TabLine      { bg = colours.bg_1 }, -- Tab pages line, not active tab page label
    TabLineFill  { bg = colours.bg }, -- Tab pages line, where there are no labels
    TabLineSel   { bg = colours.bg_2 }, -- Tab pages line, active tab page label
    Title        { fg = colours.base_3, gui = "bold" }, -- Titles for output from ":set all", ":autocmd" etc.
    Visual       { bg = colours.bg_3 }, -- Visual mode selection
    VisualNOS    { bg = colours.bg_3, gui = "bold" }, -- Visual mode selection when vim is "Not Owning the Selection".
    WarningMsg   { fg = colours.base_2 }, -- Warning messages
    Whitespace   { fg = colours.fg_3 }, -- "nbsp", "space", "tab" and "trail" in 'listchars'
    Winseparator { bg = colours.bg, fg = colours.bg_2 }, -- Separator between window splits. Inherts from |hl-VertSplit| by default, which it will replace eventually.
    WildMenu     { bg = colours.bg_2 }, -- Current match in 'wildmenu' completion

    -- Common vim syntax groups used for all kinds of code and markup.
    -- Commented-out groups should chain up to their preferred (*) group
    -- by default.
    --
    -- See :h group-name
    --
    -- Uncomment and edit if you want more specific syntax highlighting.

    Comment        { fg = colours.fg_2 }, -- Any comment

    Constant       { fg = colours.base_1 }, -- (*) Any constant
    -- String         { }, --   A string constant: "this is a string"
    -- Character      { }, --   A character constant: 'c', '\n'
    -- Number         { }, --   A number constant: 234, 0xff
    -- Boolean        { }, --   A boolean constant: TRUE, false
    -- Float          { }, --   A floating point constant: 2.3e10

    Identifier     { fg = colours.base_4 }, -- (*) Any variable name
    -- Function       { }, --   Function name (also: methods for classes)

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
    Error          { bg = colours.base_0 }, -- Any erroneous construct
    Todo           { bg = colours.base_2 }, -- Anything that needs extra attention; mostly the keywords TODO FIXME and XXX

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
    DiagnosticInfo             { fg = colours.base_4 } , -- Used as the base highlight group. Other Diagnostic highlights link to this by default (except Underline)
    DiagnosticHint             { fg = colours.fg } , -- Used as the base highlight group. Other Diagnostic highlights link to this by default (except Underline)
    -- DiagnosticVirtualTextError { } , -- Used for "Error" diagnostic virtual text.
    -- DiagnosticVirtualTextWarn  { } , -- Used for "Warn" diagnostic virtual text.
    -- DiagnosticVirtualTextInfo  { } , -- Used for "Info" diagnostic virtual text.
    -- DiagnosticVirtualTextHint  { } , -- Used for "Hint" diagnostic virtual text.
    DiagnosticUnderlineError   { fg = colours.base_0, gui = 'underline' } , -- Used to underline "Error" diagnostics.
    DiagnosticUnderlineWarn    { fg = colours.base_2, gui = 'underline' } , -- Used to underline "Warn" diagnostics.
    DiagnosticUnderlineInfo    { fg = colours.base_4, gui = 'underline' } , -- Used to underline "Info" diagnostics.
    DiagnosticUnderlineHint    { fg = colours.fg, gui = 'underline' } , -- Used to underline "Hint" diagnostics.
    -- DiagnosticFloatingError    { } , -- Used to color "Error" diagnostic messages in diagnostics float. See |vim.diagnostic.open_float()|
    -- DiagnosticFloatingWarn     { } , -- Used to color "Warn" diagnostic messages in diagnostics float.
    -- DiagnosticFloatingInfo     { } , -- Used to color "Info" diagnostic messages in diagnostics float.
    -- DiagnosticFloatingHint     { } , -- Used to color "Hint" diagnostic messages in diagnostics float.
    DiagnosticSignError        { bg = colours.bg_1, fg = colours.base_0 } , -- Used for "Error" signs in sign column.
    DiagnosticSignWarn         { bg = colours.bg_1, fg = colours.base_2 } , -- Used for "Warn" signs in sign column.
    DiagnosticSignInfo         { bg = colours.bg_1, fg = colours.base_4 } , -- Used for "Info" signs in sign column.
    DiagnosticSignHint         { bg = colours.bg_1, fg = colours.fg } , -- Used for "Hint" signs in sign column.

    -- Tree-Sitter syntax groups. Most link to corresponding
    -- vim syntax groups (e.g. TSKeyword => Keyword) by default.
    --
    -- See :h nvim-treesitter-highlights, some groups may not be listed, submit a PR fix to lush-template!
    --
    -- TSAttribute          { } , -- Annotations that can be attached to the code to denote some kind of meta information. e.g. C++/Dart attributes.
    -- TSBoolean            { } , -- Boolean literals: `True` and `False` in Python.
    -- TSCharacter          { } , -- Character literals: `'a'` in C.
    -- TSCharacterSpecial   { } , -- Special characters.
    -- TSComment            { } , -- Line comments and block comments.
    -- TSConditional        { } , -- Keywords related to conditionals: `if`, `when`, `cond`, etc.
    -- TSConstant           { } , -- Constants identifiers. These might not be semantically constant. E.g. uppercase variables in Python.
    -- TSConstBuiltin       { } , -- Built-in constant values: `nil` in Lua.
    -- TSConstMacro         { } , -- Constants defined by macros: `NULL` in C.
    -- TSConstructor        { } , -- Constructor calls and definitions: `{}` in Lua, and Java constructors.
    -- TSDebug              { } , -- Debugging statements.
    -- TSDefine             { } , -- Preprocessor #define statements.
    -- TSError              { } , -- Syntax/parser errors. This might highlight large sections of code while the user is typing still incomplete code, use a sensible highlight.
    -- TSException          { } , -- Exception related keywords: `try`, `except`, `finally` in Python.
    -- TSField              { } , -- Object and struct fields.
    -- TSFloat              { } , -- Floating-point number literals.
    -- TSFunction           { } , -- Function calls and definitions.
    -- TSFuncBuiltin        { } , -- Built-in functions: `print` in Lua.
    -- TSFuncMacro          { } , -- Macro defined functions (calls and definitions): each `macro_rules` in Rust.
    -- TSInclude            { } , -- File or module inclusion keywords: `#include` in C, `use` or `extern crate` in Rust.
    -- TSKeyword            { } , -- Keywords that don't fit into other categories.
    -- TSKeywordFunction    { } , -- Keywords used to define a function: `function` in Lua, `def` and `lambda` in Python.
    -- TSKeywordOperator    { } , -- Unary and binary operators that are English words: `and`, `or` in Python; `sizeof` in C.
    -- TSKeywordReturn      { } , -- Keywords like `return` and `yield`.
    -- TSLabel              { } , -- GOTO labels: `label:` in C, and `::label::` in Lua.
    -- TSMethod             { } , -- Method calls and definitions.
    -- TSNamespace          { } , -- Identifiers referring to modules and namespaces.
    -- TSNone               { } , -- No highlighting (sets all highlight arguments to `NONE`). this group is used to clear certain ranges, for example, string interpolations. Don't change the values of this highlight group.
    -- TSNumber             { } , -- Numeric literals that don't fit into other categories.
    -- TSOperator           { } , -- Binary or unary operators: `+`, and also `->` and `*` in C.
    -- TSParameter          { } , -- Parameters of a function.
    -- TSParameterReference { } , -- References to parameters of a function.
    -- TSPreProc            { } , -- Preprocessor #if, #else, #endif, etc.
    -- TSProperty           { } , -- Same as `TSField`.
    -- TSPunctDelimiter     { } , -- Punctuation delimiters: Periods, commas, semicolons, etc.
    -- TSPunctBracket       { } , -- Brackets, braces, parentheses, etc.
    -- TSPunctSpecial       { } , -- Special punctuation that doesn't fit into the previous categories.
    -- TSRepeat             { } , -- Keywords related to loops: `for`, `while`, etc.
    -- TSStorageClass       { } , -- Keywords that affect how a variable is stored: `static`, `comptime`, `extern`, etc.
    -- TSString             { } , -- String literals.
    -- TSStringRegex        { } , -- Regular expression literals.
    -- TSStringEscape       { } , -- Escape characters within a string: `\n`, `\t`, etc.
    -- TSStringSpecial      { } , -- Strings with special meaning that don't fit into the previous categories.
    -- TSSymbol             { } , -- Identifiers referring to symbols or atoms.
    -- TSTag                { } , -- Tags like HTML tag names.
    -- TSTagAttribute       { } , -- HTML tag attributes.
    -- TSTagDelimiter       { } , -- Tag delimiters like `<` `>` `/`.
    -- TSText               { } , -- Non-structured text. Like text in a markup language.
    TSStrong             { gui = 'bold' } , -- Text to be represented in bold.
    TSEmphasis           { gui = 'italic' } , -- Text to be represented with emphasis.
    TSUnderline          { gui = 'underline' } , -- Text to be represented with an underline.
    -- TSStrike             { } , -- Strikethrough text.
    -- TSTitle              { } , -- Text that is part of a title.
    -- TSLiteral            { } , -- Literal or verbatim text.
    -- TSURI                { } , -- URIs like hyperlinks or email addresses.
    -- TSMath               { } , -- Math environments like LaTeX's `$ ... $`
    -- TSTextReference      { } , -- Footnotes, text references, citations, etc.
    -- TSEnvironment        { } , -- Text environments of markup languages.
    -- TSEnvironmentName    { } , -- Text/string indicating the type of text environment. Like the name of a `\begin` block in LaTeX.
    -- TSNote               { } , -- Text representation of an informational note.
    -- TSWarning            { } , -- Text representation of a warning note.
    -- TSDanger             { } , -- Text representation of a danger note.
    -- TSType               { } , -- Type (and class) definitions and annotations.
    -- TSTypeBuiltin        { } , -- Built-in types: `i32` in Rust.
    -- TSVariable           { } , -- Variable names that don't fit into other categories.
    -- TSVariableBuiltin    { } , -- Variable names defined by the language: `this` or `self` in Javascript.

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
  }
end)

return theme
