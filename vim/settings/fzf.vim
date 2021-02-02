" ======================================
" --- Plug 'junegunn/fzf.vim' ---
" ======================================

map <leader>f <Esc><Esc>:Files!<CR>
inoremap <C-f> <Esc><Esc>:BLines!<CR>
map <C-g> <Esc><Esc>:BCommits!<CR>
let $FZF_DEFAULT_COMMAND='find . -not -path "*/\.git*" -type f -print'
let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.9  }  }
command! -bang -nargs=? -complete=dir Files
            \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

let $FZF_DEFAULT_OPTS="--ansi --preview-window 'right:60%' --layout reverse --margin=1,4 --preview 'bat --color=always --style=header,grid --line-range :300 {}'"
