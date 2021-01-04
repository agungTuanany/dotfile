##########################################################
#
#---------------------------------------------------------
# Agung Tuanany .bashrc file
#---------------------------------------------------------
# Last Edited   :    Mon Jan  4 04:00:20 PM WIB 2021
#
#---------------------------------------------------------
#
##########################################################


##########################################################
# DEFAULT BEHAVIORS
##########################################################
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Bash autocompletion
complete -c man which

if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

HISTCONTROL=ignoreboth


##########################################################
# DOCKER
##########################################################
# https://raw.githubusercontent.com/docker/machine/v0.16.0/contrib/completion/bash/docker-machine.bash
# -- docker-machine --
. ~/.docker-machine-completion.sh

# https://raw.githubusercontent.com/docker/compose/1.27.4/contrib/completion/bash/docker-compose
# -- docker-compose --
. ~/.docker-compose-completion.sh

##########################################################
# PS1
##########################################################
# XXX FIXME: Give colors. I'm still research
PS1='[\u@\h \W]\$ '

# # PS1="\[\e[00;34m\]\h:[\W]-> \[\e[0m\]"
# # Terminal colours (after installing GNU coreutils)
# NM="\[\033[0;38m\]" #means no background and white lines
# HI="\[\033[0;33m\]" #change this for letter colors
# HII="\[\033[0;31m\]" #change this for letter colors
# SI="\[\033[0;33m\]" #this is for the current directory
# IN="\[\033[0m\]"

# PS1='\[\033[01;34m\]\w\[\033[00m\]\$$IN '
# export PS1="$HI\u $HI\h $HI\w$HI $IN"
# export JEWELER_OPTS="--rspec --cucumber"


##########################################################
# ALIASES
##########################################################

unalias -a

# --- ls stuff ---
alias ls='ls -h --color=always'
alias ll='ls -alF --color=always --group-directories-first'
alias l='ls -lF --color=always --group-directories-first'


# --- Grep stuff ---
alias grep='grep --color=always'
alias fgrep='fgrep --color=always'
alias egrep='egrep --color=always'

# --- Less ---
alias more="less -R"

# --- Vim ---
alias vi=vim
alias nv=nvim
alias ve='vim ~/.vim/vimrc'
alias be='vim ~/.bashrc'

# --- Browser ---
alias "?"=duck

# --- Transaltor ---
alias "??"="trans :id"

# --- Tired to type source ~/.basrhc ---
alias rl="source ~/.bashrc && source ~/.bash_profile"

# --- NPM browser-sync ---
alias serve="browser-sync start --server --files ."

# --- prevent mv and cp from overriding ---
alias mv="mv -i"
alias cp="cp -i"

# --- You get bored on terminal really? ---
alias c="[ $[$RANDOM % 6] = 0 ] && timeout 2 cmatrix || clear"

# --- Change esc into caps-lock ---
alias xmp="xmodmap ~/.local/bin/speedswapper"

# --- Git aliases ---
# alias gl="git log --pretty=oneline --abbrev-commit"
alias gl="git log --graph --pretty=format:'%C(yellow)%h%Creset -%Cred%d%Creset %s %Cgreen| %cr %C(bold blue)| %an%Creset' --abbrev-commit --date=relative"
alias gs="git status --branch --short --untracked-file=all --ahead-behind"
alias gcm="git commit"

# --- cat with syntax highlighting ---
alias pcat='pygmentize -f terminal256 -O style=monokai -g'

# --- useful aliases ---
alias s=ssh
alias cx="chmod +x"
alias more=less
alias ps="ps auxf"
alias psg='ps aux | grep -v grep | grep -i -e $USER -e'
alias ..="cd .."
alias myp='ps -fjH -u $USER'
alias cleanup="rm -f *.tmp *.aux *.log"

alias myip="echo $(ip a | grep "inet " | grep -Fv 127.0.0.1 | awk '{print $2}')"
alias prettyjson='python -m json.tool'

alias rtfm=rtfm
alias mcd=mcd
alias cdl=cdl
alias backup=backup
alias gfind=gfind
alias lfind=lfind

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



# Run 'nvm use' automatically every time there's a .nvmrc file in the directory.
# Also, revert to default version when entering a directory without .nvmrc

enter_directory() {
    if [[ $PWD == $PREV_PWD ]]; then
        return
    fi

    PREV_PWD=$PWD
    if [[ -f ".nvmrc" ]]; then
        nvm use
        NVM_DIRTY=true
    elif [[ $NVM_DIRTY = true ]]; then
        nvm use default
        NVM_DIRTY=false
    fi
}

gfind() { find / -iname $@ 2>/dev/null; }
lfind() { find . -iname $@ 2>/dev/null;}

mcd() { mkdir -p $1; cd $1; }
cdl() { cd $1; ls; }
rtfm() { help $@ || man $@ || ? $@; }
backup() { cp "$1" {grep "$1" ,.bak};} #test first

##########################################################
# CUSTOM BEHAVIORS
##########################################################
set bell-style none
shopt -s checkwinsize
#shopt -s expand_aliases
#shopt -s nullglob
#shopt -s dotglob
#shopt -s extglob
shopt -s histappend
HISTZISE=10000
HISTFILESIZE=10000

# --- Horizontal alignment ---
export HRULEWIDTH=73

case "$TERM" in
    xtrem-color|*256color) color_prompt=yes;;
esac

# --- Some builtin path ---
PATH="$HOME/bin:$PATH"

# --- EDITOR ---
export EDITOR=vim
export VISUAL=vim
export EDITOR_PREFIX=vim

# -- Init xrandr ---
xrandr --output VGA-1-2 --left-of VGA-1 --auto

##########################################################
# LANGUAGES
##########################################################

# --- GO ----
export GOROOT=/usr/local/go
export PATH=$PATH:$GOROOT/bin

export GOPATH=$HOME/golib
export PATH=$PATH:$GOPATH/bin
export GOPATH=$GOPATH:$HOME/Project/go

# --- RUST ---
export PATH="$HOME/.cargo/bin${PATH:+:${PATH}}"

# --- NPM | NVM ---
source /usr/share/nvm/init-nvm.sh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


# --- cd helpers ---
. $HOME/.config/z/z.sh

