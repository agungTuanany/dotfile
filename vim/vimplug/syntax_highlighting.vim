" ============================================================================
"                           === SYNTAX HIGHLIGHTING ====
" ============================================================================
Plug 'sheerun/vim-polyglot'



" --- MARKDOWN ---
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && npm install'  }
Plug 'jtratner/vim-flavored-markdown'
" Plug 'tpope/vim-markdown'                                               " Vim Markdown runtime files
" Plug 'plasticboy/vim-markdown'                                        " Syntax highlighting, matching rules and mapping for the orginial markdown and extension


" NOTE: vim-pandoc it's make vim slow
" --- R-MARKDOWN ---
Plug 'vim-pandoc/vim-pandoc'                                            " Pandoc integration and utilities for vim
Plug 'vim-pandoc/vim-pandoc-syntax'                                     " pandoc markdown syntax, to be installed alongside vim-pandoc
Plug 'vim-pandoc/vim-rmarkdown'                                         " Rmarkdown support for vim


" --- ASCIDOCTOR ---
Plug 'habamax/vim-asciidoctor'                                          " Asciidoctor plugin for vim

" --- HTML ---
Plug 'othree/html5.vim'                                                 " HTML5 + inline SVG omnicomplete function, indent and syntax for Vim, Based on the default htmlcomplete.vim

" --- CSS ---
Plug 'ap/vim-css-color'                                                 " Preview colours in source code while editing
Plug 'hail2u/vim-css3-syntax'                                           " CSS3 syntax (and syntax defined in some way foreign specification)
Plug 'cakebaker/scss-syntax.vim'                                        " Vim syntax file for scss (Sassy CSS)
Plug 'groenewege/vim-less'                                              " Vim syntax LESS (dynamic CSS)

" --- JS ---
Plug 'pangloss/vim-javascript'
Plug 'evanleck/vim-svelte', {'branch': 'main'}                          " Svelte syntax colours
Plug 'prettier/vim-prettier', {'do': 'yarn install'}
Plug 'millermedeiros/vim-esformatter'

" --- GOLANG ---
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries'  }                     " Golang for vim,

" Racket Editor
Plug 'wlangstroth/vim-racket'

" --- SNIPMATE ---
Plug 'garbas/vim-snipmate'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'tomtom/tlib_vim'
Plug 'honza/vim-snippets'
