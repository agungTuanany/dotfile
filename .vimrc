
"=================================================================
" VIMRC SETUP BY AGUNG TUANANY
"-----------------------------------------------------------------
"SPECIAL THANKS : MILLER-MEDEIROS, DOUG-BLACK, AMIX
"=================================================================





"=================================================================
" -- BEHAVIOR --
"=================================================================
set nocompatible            " Disable vi compatible

filetype on                 " filetype must be 'on' before setting it 'off'
                            "   otherwise it exits with a bad status and breaks
                            "   git commit.
filetype off                " force reloading *after* pathogen loaded

syntax on
filetype plugin indent on   " enable detection, plugins and indent


" -- LOCAL DIRS (CENTRALIZED EVERYTHING) --
set backupdir=~/.vim/backups
set directory=~/.vim/swaps

" Change mapleader (easier to type), at the top since its used everywhere
let mapleader=","
let maplocalleader=";"

" -- LANGUAGE --
set spelllang=en_us         " spell checking
set encoding=utf-8 nobomb   " UTF-8 is awesome


" -- PERFORMANCE / BUFFER --
set hidden                  " can put buffer to the background without writing
                            "   to disk, will remember history/marks.
set lazyredraw              " don't update the display while executing macros
set ttyfast                 " Send more characters at a given time.

" -- HISTORY / FILE HANDLING --

set history=999             " Increase history (default = 20)
set undolevels=999          " More undo (default = 100)
set autoread                " Reload files if changed externally

" -- BACKUP and SWAP FILES --
set nobackup
set nowritebackup
set noswapfile

" -- SEARCH / REGEXP --

set gdefault                " RegExp global by default
set magic                   " Enable extends regexes
set hlsearch                " higlight searches
set incsearch               " Show the 'best match so far' as typed
set ignorecase smartcase    " Make searches case-insensitive, unless they 
                            "  contains upper case letters

" -- KEYS --
set backspace=indent,eol,start  " Allow backspacing over everything.
set esckeys                     " Allow cursor keys in insert mode.
set nostartofline               " Make j/k respect the columns
set virtualedit=all             " Allow the cursor to go in to 'invalid' places
set timeoutlen=500              " How long it wait for mapped commands
set ttimeoutlen=100             " Faster tiemout for escape key and others

" Use a bar-shaped cursor for insert mode, even through tmux.
if exists('$TMUX')
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

                 

"=================================================================
" -- COLOR AND UI --
"=================================================================

syntax enable
"colorscheme badwolf
"set termguicolors

" make 'var' keyword easier to spot
hi link javascriptType keyword
" default ColorColumn is too distractive
hi clear ColorColumn
hi link ColorColumn FoldColumn
" default line number is too distractive
hi clear LinerNr
hi link LineNr Comment
hi link Overlength Error

" -- UI SETTING --

set cursorline              " Higlight current line.
set laststatus=2            " Always show status line.
set number                  " enable line number.
set numberwidth=5           " width of numbers line (default on gvim is 4)
set report=0                " show all changes.
set showcmd                 " show partial command on last line of screen.
set showmatch               " show matching parenthesis.
set title                   " show the flename in the window title bar.
set splitbelow splitright   " how to split new window

set scrolloff=5             " Start scrolling n lines before horizontal
                            "  border of window
set sidescrolloff=7         " Start scrolling n chars before end of screen
set sidescroll=1            " The minimal number of columns to scroll horizontally


" add useful stuff to title bar (file name, flags, cwd)
" " based on @factorylabs
if has('title') && (has('gui_running') || &title)
    set titlestring=
    set titlestring+=%f
    set titlestring+=%h%m%r%w
    set titlestring+=\ -\ %{v:progname}
    set titlestring+=\ -\ %{substitute(getcwd(),\ $HOME,\'~',\ '')}
endif

" use relative line number by default
if exists('+relativenumber')
 set relativenumber
endif

" -- COMMAND COMPLETION --
set wildmenu                " HITTING TAB in command mode will show possible completion
set wildchar=<TAB>
set wildmode=list:longest
set wildignore+=*.DS_STORE,*.db,node_modules/**,*.jpg,*.png,*.gif
set lazyredraw
set nocursorline
set fillchars+=vert:┃       "find this setup

" -- FOLDING --
 set foldmethod=manual      " manual fold.
 set foldnestmax=3          " deepest fold is 3 levels
 set nofoldenable           " don't fold by default

" -- DIFF --
set diffopt=filter          " Add vertical spaces to keep rigth and left aligned.
set diffopt+=iwhite         " Ignore whitespace changes

"-- LIST CHARS --
" List spaces and tabs to avoid trailing spaces and mixed indentation
" see key napping at the end of file to toggle 'list'
set listchars=tab:▹\ ,trail:·,nbsp:⚋,eol:¬
set fillchars=fold:-
set list

" --- remove sounds effects ---
set noerrorbells
set visualbell


"=================================================================
"-- SPACE, INDENTATION, TEXT-WRAP AND TABS --
"=================================================================
set expandtab               " Expand tabs to spaces
set tabstop=4
set expandtab
set softtabstop=4
set shiftwidth=4
set nomodeline              " Security issue don't use modeline
set autoindent smartindent  " auto/smart indent
set textwidth=80
set wrap
set smarttab                " At start of line, <Tab> insert shift width 
                            "  spaces, <Bs>, deletes shift width spaces.
set formatoptions=qrn1      " auto formatting
set formatoptions-=o        " don't start new lines w/ comment leader on
                            "   pressing 'o'

" move to beginning/end of line
nnoremap B ^
nnoremap E $

" $/^ doesn't do anything
nnoremap $ <nop>
nnoremap ^ <nop>

" Escape with 
inoremap jk <esc>

"-- SMART WAY TO MOVE BETWEEN WINDOW (<ctrl>j) --
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

"-- USEFUL MAPPING FOR MANAGING TABS --
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove 

" Opens a new tab with the current buffer's path
" Super useful when editing files in the same directory
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/


"=================================================================
"-- LEADER SHORCUT / KEY MAPPING --
"=================================================================
" mapleader was set at the top of the file to avoid conflicts

" faster commands
nnoremap <spaces> :

" -- FIX / IMPORVE DEFAULT BEHAVIOR --
nnoremap <leader>sv :source $MYVIMRC<CR>
nnoremap <leader><space> :noh<CR>


"sudo write
command! W w !sudo tee % > /dev/null


"Swap v and CTRL-V, because Block mode is more useful that Visual mode
nnoremap    v   <C-V>
nnoremap <C-V>     v
vnoremap    v   <C-V>
vnoremap <C-V>     v

" avoid mistyping commands
command! W w
command! Wq wq
command! Bd bd

void mistyping commands
command! W w
command! Wq wq
command! Bd bd

" Split line (sister to [J]oin lines)
" The normal use of S is covered by cc, so don't worry about shadowing
 nnoremap S i<cr><esc><right>

" movement by screen line instead of file line (for text wrap)
 nnoremap j gj
 nnoremap <down> gj
 nnoremap k gk
 nnoremap <up> gk

" next tab
 nnoremap <C-Tab> :tabn<CR>

"  automatic esc, really uncommon to type jj,
inoremap jj <ESC>
inoremap jk <Esc>

" Faster scrolling
 nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>







