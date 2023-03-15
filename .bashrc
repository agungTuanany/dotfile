##
#
# Author               : Agung Tuanany
# Last Modified        : Wed Jan 4 11:06:19 AM WIB 2023
#
# RECOMMEND USE:
#
# - Use symbolic links from this repo.
# if you want to use symbolic links, make sure to create them first:
#
# ln -sf $HOME/Repo/agung_dotfile/.xinitrc $HOME/.xinitrc
#
# - Plan:
# copy this file into your home directory.
#
# REPO: https://github.com/agungTuanany/dotfile.git
##

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
alias igrep='grep -i --color'
alias bashrc='nvim ~/.bashrc'

alias pacupdate='sudo pacman -Syu'
alias pacclean='sudo pacman -Qtdq | sudo pacman -Rns -'

#alias nvimrc='cd ~/.config/nvim && nvim ~/.config/nvim'
alias nvimrc='nvim ~/.config/nvim'
alias tmuxrc='nvim ~/.tmux.conf'

alias duck=duck
alias archwiki='lynx wiki.archlinux.org'

alias rl='source ~/.bashrc'
alias dmesg='sudo dmesg -T -x'
alias lspcv='lspci -k | grep -A 2 -E "(VGA|3D)"'

# GIT ALIASES
# alias gl="git log --pretty=oneline --abbrev-commit"
alias gl="git log --graph --pretty=format:'%C(yellow)%h%Creset -%Cred%d%Creset %s %Cgreen| %cr %C(bold blue)| %an%Creset' --abbrev-commit --date=relative"
alias gs="git status --branch --short --untracked-file=all --ahead-behind"
alias gcm="git commit"

## dwm setup
alias dwmrc='nvim ~/.config/dwm-6.4/config.def.h'
alias dwmcp='sudo cp -rf ~/.config/dwm-6.4/config.def.h  ~/.config/dwm-6.4/config.h'
alias dwmake='cd ~/.config/dwm-6.4 && sudo make install clean && cd -'

## dmenu setup
alias dmenurc='nvim ~/.config/dmenu/config.def.h'
alias dmenucp='sudo cp -rf ~/.config/dmenu/config.def.h ~/.config/dmenu/config.h'
alias dmenumake='cd ~/.config/dmenu && sudo make install clean && cd -'

## st setup
alias strc='nvim ~/.config/st/config.def.h'
alias stcp='sudo cp -rf ~/.config/st/config.def.h ~/.config/st/config.h'
alias stmake='cd ~/.config/st && sudo make install clean && cd -'

## translate-shell
alias trid='trans en:id'
alias tren='trans id:en'

# offline dictionary with dictd
alias dictc='dict -d foldoc '
alias dictgc='dict -d gcide '
alias dictsyn='dict -d moby-thesaurus '
alias dictwn='dict -d wn '
alias rtfm=rtfm

##########################################################
# FUNCTIONS
##########################################################
urlencode () {
    local str="$*"
    local encoded=""
    local i c x
    for (( i=0; i<${#str}; i++ )); do
        c=${str:$i:1}
        case "$c" in
            [-_.~a-zA-Z0-9] ) x="$c" ;;
            # `'$c` see https://pubs.opengroup.org/onlinepubs/009695399/utilities/printf.html
            * ) printf -v x '%%%02x' "'$c" ;;
        esac
        encoded+="$x"
    done
    echo "$encoded"
}

duck () {
    local url=$(urlencode "$*")
    lynx "https://duckduckgo.com/lite?q=$url"
}


rtfm() { help $@ || man $@ || ? $@; }

##########################################################
# ENDS FUNCTIONS
##########################################################

# require to install fzf
source /usr/share/fzf/key-bindings.bash
source /usr/share/fzf/completion.bash
[[ -s "$HOME/Repo/qfc/bin/qfc.sh" ]] && source "$HOME/Repo/qfc/bin/qfc.sh"

## TEMPORARY
##alias aml_flash='./home/daun/.local/bin/aml-flash'

VBOX_USB=usbfs

## RUST SETUP
. "$HOME/.cargo/env"
alias rustrl='. "$HOME/.cargo/env"'

# rupa/z - jump around
. $HOME/Repo/z/z.sh

# for emacs 25+ prerequire tree-sitter
PATH=$HOME/.cask/bin:$PATH
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"
