" ============================================================================
"                            === SYS UTILS ====
" ============================================================================

Plug 'junegunn/vim-plug'                                        " Register vim-plug for `:h plug-options`
Plug 'ctrlpvim/ctrlp.vim'                                       " Full path Fuzzy file, buffer, mru, tag .. Finder for vim

Plug 'scrooloose/nerdtree'                                      " A File system explorer

Plug 'junegunn/fzf', { 'do': { -> fzf#install()}}               " FZF latest binary
Plug 'junegunn/fzf.vim'                                         " FZF

Plug 'christoomey/vim-tmux-navigator'                           " Seamless navigation between tmux panes and vim split

Plug 'terryma/vim-multiple-cursors'                             " Multiple selection
Plug 'jiangmiao/auto-pairs'                                     " Insert, or delete bracket, parens, quotes in pair
Plug 'ervandew/supertab'                                        " Vim plugin which allow to use <Tab> for all insert completion needs (:help ins-completion)

" --- BUFFER ---
Plug 'vim-scripts/bufkill.vim'                                  " Unload/delete/wipe a buffer, keep its window(s), display last accessed buffer(s)
Plug 'sjl/gundo.vim'                                            " Visualize your changes with Tree

" MISCELLANEOUS
" Plug 'neoclide/coc.nvim'                                        " Make vim smart as VSCode
