" --- Plug 'wlangstroth/vim-racket' ---
if has("autocmd")
    au BufReadPost *.rjtm *.rktl set filetype=racket
    au filetype racket set lisp
    au filetype racket set autoindent
endif
