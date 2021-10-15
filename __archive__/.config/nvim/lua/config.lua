local M = {}

local o = vim.o -- same as ':set'
local b = vim.bo -- same as ':setlocal'
local g = vim.g -- get or set an 'option' like ':set' only 'global_value'
local w = vim.wo-- same as ':setlocal'
local cmd = vim.cmd
local opt = vim.opt -- same as 'set-option'

local api =  vim.api

--[[{{{ 2. Defaults                nvim-defaults

- Syntax highlighting is enabled by default

- ":filetype plugin indent on"  is enabled by default
- autoindent                    is enabled
- autoread                      is enabled
- background                    defaults to "dark" (unless set automatically by the terminal/UI)
- backspace                     defaults to "indent,eol,start"
- backupdir                     defaults to .,~/.local/share/nvim/backup (xdg)
- belloff                       defaults to "all"
- compatible                    is always disabled
- complete                      excludes "i"
- cscopeverbose                 is enabled
- directory                     defaults to ~/.local/share/nvim/swap// (xdg), auto-created
- display                       defaults to "lastline,msgsep"
- encoding                      is UTF-8 (cf. 'fileencoding' for file-content encoding)
- fillchars                     defaults (in effect) to "vert:│,fold:·,sep:│"
- formatoptions                 defaults to "tcqj"
- fsync                         is disabled
- history                       defaults to 10000 (the maximum)
- hlsearch                      is enabled
- incsearch                     is enabled
- langnoremap                   is enabled
- langremap                     is disabled
- laststatus                    defaults to 2 (statusline is always shown)
- listchars                     defaults to "tab:> ,trail:-,nbsp:+"
- nrformats                     defaults to "bin,hex"
- ruler                         is enabled
- sessionoptions                includes "unix,slash", excludes "options"
- shortmess                     includes "F", excludes "S"
- showcmd                       is enabled
- sidescroll                    defaults to 1
- smarttab                      is enabled
- startofline                   is disabled
- tabpagemax                    defaults to 50
- tags                          defaults to "./tags;,tags"
- ttimeoutlen                   defaults to 50
- ttyfast                       is always set
- viewoptions                   includes "unix,slash"
- undodir                       defaults to ~/.local/share/nvim/undo (xdg), auto-created
- viminfo                       includes "!"
- wildmenu                      is enabled
- wildoptions                   defaults to "pum,tagfile"

- man.vim plugin is enabled, so :Man is available by default.
- matchit plugin is enabled. To disable it in your config:
:let loaded_matchit = 1

- g:vimsyn_embed defaults to "l" to enable Lua highlighting
-- }}}]]


function M.default()
    M.set_option()
    M.set_option_tabs()
    M.set_option_scrolling()
    M.set_option_buffer()
    M.set_option_fileHandling()
    M.set_option_search_regexp()
    M.set_option_keys()
    M.set_option_ui()
    M.set_option_command_completion()
    M.set_option_fold_method()


end

function M.set_option()
    opt.hidden              = true

    opt.wrap                = false
    opt.autoread            = true
    opt.spell               = true
end


function M.set_option_tabs()
    --opt.smarttab              = true     -- enable by default
    opt.tabstop                 = 4
    opt.shiftwidth              = 4
    opt.softtabstop             = 4

    opt.copyindent              = true
    opt.expandtab               = true
    opt.wrap                    = false
end

function M.set_option_text_wrap()
    opt.nomodeline              = true
end

function M.set_option_scrolling()

    opt.scrolloff               = 8
    opt.sidescrolloff           = 15
    opt.spell.spelllang         = en_us
end

function M.set_option_buffer()
    opt.hidden                  = true
    opt.lazyredraw              = true
    -- opt.ttyfast                 = true   -- enable by default
end

function M.set_option_fileHandling()
    opt.backup                  = false
    opt.writebackup             = false
    opt.swapfile                = false
end

function M.set_option_search_regexp()
    opt.gdefault                = true
    opt.magic                   = true
    opt.smartcase               = true
    opt.ignorecase              = true
    print("search regexp reloaded")
end

function M.set_option_keys()
    opt.startofline             = false
    opt.timeoutlen              = 500           -- time in milisecond to wait for 'mapped key sequence' to complete
    opt.ttimeoutlen             = 100           -- time in milisecond to wait for a 'key code sequence' to complete
end

function M.set_option_ui()
    --  'opt' is not a valid function to call to set "t_Co" on neovim.
    --opt.t_Co = 256
    opt.termguicolors       = true
    g.t_co                  = 256

    -- --XX NOTE: XX--
    -- I cannot find the Lua-highlight function. It just provide two option
    -- "vim.highlight.on_yank()" and "vim.highlight.range()"
    --
    -- make 'var' keyword easier to spot
    --hi link javascriptType Keyword
    -- XX ENDNOTE XX

    --opt.cursorline = true
    --opt.cursorcolumn = true

    opt.numberwidth             = 5
    opt.number                  = true
    opt.relativenumber          = true
    opt.report                  = 0
    opt.showmode                = true
    opt.splitbelow              = true
    opt.splitright              = true

    opt.showmatch               = true      -- show matching parenthesis
    opt.showmode                = true      -- show the current mode
    opt.title                   = true      -- show the filename in the window title bar

    opt.scrolloff               = 10         -- start scorlling 'n' lines before horizontal
    opt.sidescrolloff           = 10         -- start scrolling 'n' lines before vertical of screen

    print ("set_option_ui - function reloaded")
end


function M.set_option_command_completion()
    g.wildchar                  = "<TAB>"
    opt.wildmode                = "list:longest"
    opt.wildignore              ="*.DS_STORE, *.db, node_modules/**, *.jpg, *.png, *.gif"

end

function M.set_option_fold_method()
    opt.foldmethod              = "marker"
    opt.foldnestmax             = 3

    -- TODO: change the foldcolumn guibg into none
    --FoldColumn     xxx ctermfg=14 ctermbg=242 guifg=Cyan guibg=Grey
end

return M
