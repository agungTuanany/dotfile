" ======================================
" ALL PLUGIN CONFIGURATION
" ======================================


" This file contains the list of plugin installed using vim-plug plugin manager.
" Once you've updated the list of plugin, you can run plug updated by run the
" command :PlugUpdate; from within vim directly invoking it to.

set rtp+=~/.vim/vimplug/ "Submodels

call plug#begin('~/.vim/plugged')

" Split up plugins by category into smaller files.
" To reduces messiness, reload time and makes it easier to fork. 
" See ~/.vim/vimplug to edit them

runtime sys_utils.vim
runtime practically.vim
runtime syntax_highlighting.vim
runtime asthetic.vim
runtime icebox.vim
runtime vim_improvement.vim
runtime search.vim
runtime project.vim


call plug#end()

filetype plugin indent on
