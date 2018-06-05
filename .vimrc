
"========================================
" VIMRC SETUP BY AGUNG TUANANY
"========================================

" -- COLOR --
syntax enable
"colorscheme badwolf
"set termguicolors

"-- SPACE AMD TABS --
set tabstop=4
set expandtab
set softtabstop=4
set shiftwidth=4
set modelines=1
filetype indent on
filetype plugin on
set autoindent

"-- UI LAYOUT --
set number
set showcmd
set nocursorline
set wildmenu
set lazyredraw
set showmatch
set fillchars+=vert:┃       "find this setup

"-- SEARCHING --
set ignorecase
set incsearch
set hlsearch
nnoremap <leader><space> :nohlsearch<CR>    "turn off search higlight



"--FOLDING --
set foldmethod=indent
set foldnestmax=10
set foldenable
nnoremap <space> za
"f search higlight
set foldlevelstart=10

"--LINE SHORCUTS --
nnoremap j gj
nnoremap k gk
nnoremap gV `[v`] "V all file

" move to beginning/end of line
nnoremap B ^
nnoremap E $

" $/^ doesn't do anything
nnoremap $ <nop>
nnoremap ^ <nop>

inoremap jk <esc>

"-- SMART WAY TO MOVE BETWEEN WINDOW (<ctrl>j) --
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

"-- USEFUL MAPPING FOR MANANGIG TABS --
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove 

" Opens a new tab with the current buffer's path
" Super useful when editing files in the same directory
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/


"-- LEADER SHORCUT --
let mapleader=","
nnoremap <leader>sv :source $MYVIMRC<CR>
nnoremap <leader><space> :noh<CR>
