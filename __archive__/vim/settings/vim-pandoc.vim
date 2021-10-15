" ======================================
" --- Plug 'vim-pandoc/vim-pandoc' ---
" ======================================
let g:pandoc#modules#disables = ['folding']

" Pandoc highlight markdown
augroup pandoc
    au!
    au BufNewFile,BufFilePre,BufRead *.md,*.markdown set filetype=markdown
    autocmd! FileType vimwiki set syntax=markdown

    au BufNewFile,BufRead,BufWrite *.md syntax match Comment /\%^---\_.\{-}---$/
    autocmd FileType markdown let b:sleuth_automatic=0
    autocmd FileType markdown set conceallevel=0
    autocmd FileType markdown normal zR
augroup END
