#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# run this xmodmap to change caps-lock into Ctr-Left

PS1='[\u@\h \W]\$ '


# Aliases
alias ls='ls --color=auto'
alias ll='ls -lah'

alias cp='cp -r'
alias rm='rm -rf'

alias e_key='xmodmap ~/.local/bin/emacs_keyboard'
alias xmonitor='~/.local/bin/xmonitor'
alias xinitrc='nvim ~/.xinitrc'

alias _xstart='e_key && xmonitor'

alias c='clear'
alias igrep='grep -i'
alias bashrc='nvim ~/.bashrc'

alias pacupdate='sudo pacman -Syu'

#alias nvimrc='cd ~/.config/nvim && nvim ~/.config/nvim'
alias nvimrc='nvim ~/.config/nvim'
alias tmuxrc='nvim ~/.config/.tmux.conf'

alias duck='lynx duckduckgo.com'
alias arch='lynx wiki.archlinux.org'

alias rl='source ~/.bashrc'

# GIT ALIASES
# alias gl="git log --pretty=oneline --abbrev-commit"
alias gl="git log --graph --pretty=format:'%C(yellow)%h%Creset -%Cred%d%Creset %s %Cgreen| %cr %C(bold blue)| %an%Creset' --abbrev-commit --date=relative"
alias gs="git status --branch --short --untracked-file=all --ahead-behind"
alias gcm="git commit"

alias dwmrc='nvim ~/.config/dwm-6.4/config.def.h'
alias dwmcp='sudo cp -rf ~/.config/dwm-6.4/config.def.h  ~/.config/dwm-6.4/config.h'
alias dwmake='cd ~/.config/dwm-6.4 && sudo make install clean'

alias trid='trans en:id'
alias tren='trans id:en'

## TEMPORARY
##alias aml_flash='./home/daun/.local/bin/aml-flash'

# rupa/z - jump around
. $HOME/Repo/z/z.sh

VBOX_USB=usbfs

## RUST SETUP
. "$HOME/.cargo/env"

alias rustrl='. "$HOME/.cargo/env"'
