require "agung"

------------------------------------------------------------------------------------
--  Author:                     Agung Tuanany
--  Last Date Modified:         Mon Oct 25 09:40:04 PM WIB 2021
--  Credit:                     #wincent, #ThePrimeagen #tjdevries
------------------------------------------------------------------------------------

------------------------------------------------------------------------------------
-- OPTIONS {{{1
------------------------------------------------------------------------------------

local home = vim.env.HOME
local config = home .. '/.config/nvim'

vim.opt.autoindent     = true                               -- maintain indent of current line from previous line
vim.opt.backspace      = 'indent,start,eol'                 -- allow unrestricted backspacing in insert mode
vim.opt.backup         = false                              -- don't make backups before writing
vim.opt.backupcopy     = 'yes'                              -- overwrite files to update, instead of renaming + rewriting
vim.opt.backupdir      = config .. '/backup//'              -- keep backup files out of the way (ie. if 'backup' is ever set)
vim.opt.backupdir      = vim.opt.backupdir + '.'            -- fallback
vim.opt.backupskip     = vim.opt.backupskip + '*.re,*.rei'  -- prevent bsb's watch mode from getting confused (if 'backup' is ever set)
vim.opt.belloff        = 'all'                              -- never ring the bell for any reason
vim.opt.completeopt    = 'menu'                             -- change defaault value do not use 'preview' (for nvim-compe)
vim.opt.completeopt    = vim.opt.completeopt + 'menuone'    -- show menu even if there is only one candidate (for nvim-compe)
vim.opt.completeopt    = vim.opt.completeopt + 'noselect'   -- don't automatically select canditate (for nvim-compe)
--vim.opt.completeopt    = vim.opt.completeopt + 'noinsert'  -- don't automatically insert canditate
vim.opt.cursorline     = false                              -- highlight current line
vim.opt.diffopt        = vim.opt.diffopt + 'foldcolumn:0'   -- don't show fold column in diff view
vim.opt.directory      = config .. '/nvim/swap//'           -- keep swap files out of the way
vim.opt.directory      = vim.opt.directory + '.'            -- fallback
vim.opt.emoji          = false                              -- don't assume all emoji are double width
vim.opt.expandtab      = true                               -- always use spaces instead of tabs
vim.opt.fillchars      = {
    diff                 = '∙',                              -- BULLET OPERATOR (U+2219, UTF-8: E2 88 99)
    eob                  = ' ',                              -- NO-BREAK SPACE (U+00A0, UTF-8: C2 A0) to suppress ~ at EndOfBuffer
    fold                 = '·',                              -- MIDDLE DOT (U+00B7, UTF-8: C2 B7)
    vert                 = '┃',                              -- BOX DRAWINGS HEAVY VERTICAL (U+2503, UTF-8: E2 94 83)
}
vim.opt.foldlevelstart = -1                                 -- start folded
vim.opt.foldmethod     = 'marker'                           -- not as cool as syntax, but faster
vim.opt.foldtext       = 'v:lua.agung.foldtext()'
vim.opt.formatoptions  = vim.opt.formatoptions + 'j'        -- remove comment leader when joining comment lines
vim.opt.formatoptions  = vim.opt.formatoptions + 'n'        -- smart auto-indenting inside numbered lists
vim.opt.guifont        = 'Source Code Pro Light:h13'
vim.opt.guicursor      = 'a:blinkon0'                       -- disable blink on cursor
vim.opt.hidden         = true                               -- allows you to hide buffers with unsaved changes without being prompted
vim.opt.inccommand     = 'split'                            -- live preview of :s results
vim.opt.joinspaces     = false                              -- don't autoinsert two spaces after '.', '?', '!' for join command
vim.opt.laststatus     = 2                                  -- always show status line
vim.opt.lazyredraw     = true                               -- don't bother updating screen during macro playback
vim.opt.linebreak      = true                               -- wrap long lines at characters in 'breakat'
vim.opt.list           = true                               -- show whitespace
vim.opt.listchars      = {
    nbsp                 = '⦸',                              -- CIRCLED REVERSE SOLIDUS (U+29B8, UTF-8: E2 A6 B8)
    extends              = '»',                              -- RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK (U+00BB, UTF-8: C2 BB)
    precedes             = '«',                              -- LEFT-POINTING DOUBLE ANGLE QUOTATION MARK (U+00AB, UTF-8: C2 AB)
    tab                  = '▷⋯',                             -- WHITE RIGHT-POINTING TRIANGLE (U+25B7, UTF-8: E2 96 B7) + MIDLINE HORIZONTAL ELLIPSIS (U+22EF, UTF-8: E2 8B AF)
    trail                = '•',                              -- BULLET (U+2022, UTF-8: E2 80 A2)
}

if vi then
    vim.opt.loadplugins = false
end

vim.opt.modelines      = 5      -- scan this many lines looking for modeline
vim.opt.number         = true   -- show line numbers in gutter
vim.opt.pumblend       = 10     -- pseudo-transparency for popup-menu
vim.opt.relativenumber = true   -- show relative numbers in gutter
vim.opt.scrolloff      = 10     -- start scrolling 10 lines before edge of viewport

--[[
if root then
vim.opt.shada     = '' -- Don't create root-owned files.
vim.opt.shadafile = 'NONE'
else
-- Defaults:
--   Neovim: !,'100,<50,s10,h
--
-- - ! save/restore global variables (only all-uppercase variables)
-- - '100 save/restore marks from last 100 files
-- - <50 save/restore 50 lines from each register
-- - s10 max item size 10KB
-- - h do not save/restore 'hlsearch' setting
--
-- Our overrides:
-- - '0 store marks for 0 files
-- - <0 don't save registers
-- - f0 don't store file marks
-- - n: store in ~/.config/nvim/
--
vim.opt.shada = ''0,<0,f0,n~/.config/nvim/shada'
end
--]]

vim.opt.shell         = 'sh'                    -- shell to use for `!`, `:!`, `system()` etc.
vim.opt.shiftround    = false                   -- don't always indent by multiple of shiftwidth
vim.opt.shiftwidth    = 4                       -- spaces per tab (when shifting)
vim.opt.shortmess     = vim.opt.shortmess + 'A' -- ignore annoying swapfile messages
vim.opt.shortmess     = vim.opt.shortmess + 'I' -- no splash screen
vim.opt.shortmess     = vim.opt.shortmess + 'O' -- file-read message overwrites previous
vim.opt.shortmess     = vim.opt.shortmess + 'T' -- truncate non-file messages in middle
vim.opt.shortmess     = vim.opt.shortmess + 'W' -- don't echo "[w]"/"[written]" when writing
vim.opt.shortmess     = vim.opt.shortmess + 'a' -- use abbreviations in messages eg. `[RO]` instead of `[readonly]`
vim.opt.shortmess     = vim.opt.shortmess + 'c' -- completion messages
vim.opt.shortmess     = vim.opt.shortmess + 'o' -- overwrite file-written messages
vim.opt.shortmess     = vim.opt.shortmess + 't' -- truncate file messages at start
vim.opt.showbreak     = '↳ '                    -- DOWNWARDS ARROW WITH TIP RIGHTWARDS (U+21B3, UTF-8: E2 86 B3)
vim.opt.showcmd       = true                    -- don't show extra info at end of command line
vim.opt.sidescroll    = 0                       -- sidescroll in jumps because terminals are slow
vim.opt.sidescrolloff = 3                       -- same as scrolloff, but for columns
vim.opt.smarttab      = true                    -- <tab>/<BS> indent/dedent in leading whitespace

if not vi then
    vim.opt.softtabstop = -1 -- use 'shiftwidth' for tab/bs at end of line
end

vim.opt.spellcapcheck = ''                      -- don't check for capital letters at start of sentence
vim.opt.splitbelow    = true                    -- open horizontal splits below current window
vim.opt.splitright    = true                    -- open vertical splits to the right of the current window
vim.opt.suffixes      = vim.opt.suffixes - '.h' -- don't sort header files at lower priority
vim.opt.swapfile      = false                   -- don't create swap files
vim.opt.switchbuf     = 'usetab'                -- try to reuse windows/tabs when switching buffers
vim.opt.synmaxcol     = 200                     -- don't bother syntax highlighting long lines
vim.opt.tabstop       = 4                       -- spaces per tab
vim.opt.termguicolors = true                    -- use guifg/guibg instead of ctermfg/ctermbg in terminal
vim.opt.textwidth     = 85                      -- automatically hard wrap at 85 columns

if root then
    vim.opt.undofile = false -- don't create root-owned files
else
    vim.opt.undodir  = config .. '/undo/'       -- keep undo files out of the way
    vim.opt.undodir  = vim.opt.undodir + '.'    -- fallback
    -- vim.opt.undofile = true                    -- actually use undo files
    vim.opt.undofile = false                    -- do I need undofile?
end

vim.opt.updatetime  = 2000                                      -- CursorHold interval
vim.opt.updatecount = 0                                         -- update swapfiles every 80 typed chars
vim.opt.viewdir     = config .. '/view'                         -- where to store files for :mkview
vim.opt.viewoptions = 'cursor,folds'                            -- save/restore just these (with `:{mk,load}view`)
vim.opt.virtualedit = 'block'                                   -- allow cursor to move where there is no text in visual block mode
vim.opt.visualbell  = true                                      -- stop annoying beeping for non-error errors
vim.opt.whichwrap   = 'b,h,l,s,<,>,[,],~'                       -- allow <BS>/h/l/<Left>/<Right>/<Space>, ~ to cross line boundaries
vim.opt.wildcharm   = 26                                        -- ('<C-z>') substitute for 'wildchar' (<Tab>) in macros
vim.opt.wildignore  = vim.opt.wildignore + '*.o,*.rej,*.so'     -- patterns to ignore during file-navigation
vim.opt.wildignore  = vim.opt.wildignore + '*.pyc'              -- patterns to ignore during file-navigation
vim.opt.wildignore  = vim.opt.wildignore + '*_build/*'          -- patterns to ignore during file-navigation
vim.opt.wildignore  = vim.opt.wildignore + '**/coverage/*'      -- patterns to ignore during file-navigation
vim.opt.wildignore  = vim.opt.wildignore + '**/node_modules'    -- patterns to ignore during file-navigation
vim.opt.wildignore  = vim.opt.wildignore + '**/android/*'       -- patterns to ignore during file-navigation
vim.opt.wildignore  = vim.opt.wildignore + '**/ios/*'           -- patterns to ignore during file-navigation
vim.opt.wildignore  = vim.opt.wildignore + '**/.git/*'          -- patterns to ignore during file-navigation
vim.opt.wildmenu    = true                                      -- show options as list when switching buffers etc
vim.opt.wildmode    = 'longest:full,full'                       -- shell-like autocomplete to unambiguous portion
vim.opt.winblend    = 10                                        -- psuedo-transparency for floating windows
vim.opt.wrap        = false                                     -- swictch off the default wrap
vim.opt.writebackup = false                                     -- don't keep backups after writing

-- TODO: move this to autocmd
vim.api.nvim_exec([[
    augroup new_highlight
        autocmd!
        highlight Folded guibg=none
        highlight Pmenu cterm=italic ctermfg=121 gui=bold guibg=None guifg=#ff80ff
        highlight SignColumn guibg=None
    augroup end
]], false)

-- Highlight up to 255 columns (this is the current Vim max) beyond 'textwidth'
--[[
agung.vim.setlocal(
'colorcolumn',
'+' .. agung.util.join(agung.util.range(0, 254), ',+')
)
--]]
--}}}
------------------------------------------------------------------------------------

-- load mapping
vim.g.mapleader = ","
vim.g.maplocalleader = ";"
require "agung.mapping"

-- load helpers function
require "agung.globals"

-- load plugins
require "agung.plugins"

-- load lsp config
require "agung.lsp"

-- load telescope config
require "agung.telescope"

-- load treesitter config
-- require "agung.treesitter"

-- load gitsigns
require "agung.gitsigns"
