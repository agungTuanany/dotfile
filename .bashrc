##########################################################
#
#---------------------------------------------------------
# Agung Tuanany .bashrc file
#---------------------------------------------------------
# Last Edited       : Fri Nov  5 11:11:40 PM WIB 2021
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
# POWERLINE
##########################################################


powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
source /usr/lib/python3.9/site-packages/powerline/bindings/bash/powerline.sh

# Powerline configuration
#if [ -f /usr/lib/python3.9/site-packages/powerline/bindings/bash/powerline.sh ]; then
#	#powerline-daemon -q
#	#POWERLINE_BASH_CONTINUATION=1
#	#POWERLINE_BASH_SELECT=1
#	#source /usr/share/powerline/bindings/bash/powerline.sh
#	echo "powerline defined";
#else
#	echo "powerline not defined"
#fi

##########################################################
# PS1
##########################################################

#function _update_ps1() {
#    PS1=$(powerline-shell $?)
#}

#if [[ $TERM != linux && ! $PROMPT_COMMAND =~ _update_ps1 ]]; then
#    PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"
#fi


##########################################################
# DOCKER
##########################################################
# https://raw.githubusercontent.com/docker/machine/v0.16.0/contrib/completion/bash/docker-machine.bash
# -- docker-machine --
## ~/.docker-machine-completion.sh

# https://raw.githubusercontent.com/docker/compose/1.27.4/contrib/completion/bash/docker-compose
# -- docker-compose --
## . ~/.docker-compose-completion.sh

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
alias ls='exa -h --color=always'
alias ll='ls -alF --color=always --group-directories-first'
alias l='ls -lF --color=always --group-directories-first'


# --- Grep stuff ---
#alias grep='rg --color=always'
alias fgrep='fgrep --color=always'
alias egrep='egrep --color=always'

# --- Less ---
alias more="less -R"

# --- Vim ---
alias vi=nvim
alias nv=nvim
alias vimrc='nv ~/.vim/vimrc'
alias nvimrc='nv ~/.config/nvim/init.lua'
alias bashrc='nv ~/.bashrc'
alias tmuxrc='nv ~/.tmux.conf'
alias vprofile="python <(curl -sSL https://raw.githubusercontent.com/hyiltiz/vim-plugins-profile/master/vim-plugins-profile.py)"

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
# alias c="[ $[$RANDOM % 6] = 0 ] && timeout 2 cmatrix || clear"
alias c="clear"

# --- Change esc into caps-lock ---
alias xmp="xmodmap ~/.local/bin/speedswapper"

# --- Git aliases ---
# alias gl="git log --pretty=oneline --abbrev-commit"
alias gl="git log --graph --pretty=format:'%C(yellow)%h%Creset -%Cred%d%Creset %s %Cgreen| %cr %C(bold blue)| %an%Creset' --abbrev-commit --date=relative"
alias gs="git status --branch --short --untracked-file=all --ahead-behind"
alias gcm="git commit"

# --- cat with syntax highlighting ---
alias pcat='pygmentize -f terminal256 -O style=monokai -g'
alias cat="bat $1"

# --- useful aliases ---
alias s=ssh
alias cx="chmod +x"
alias more=less
alias ps="ps auxf"
alias psg='ps aux | grep -v grep | grep -i -e $USER -e'
alias ..="cd .."
alias myp='ps -fjH -u $USER'
alias cleanup="rm -f *.tmp *.aux *.log"
alias igrep="grep -i --color=always"

alias myip="echo $(ip a | grep "inet " | grep -Fv 127.0.0.1 | awk '{print $2}')"
alias prettyjson='python -m json.tool'

alias resolv="sudo vim /etc/resolv.conf"
alias mountsdb="sudo mount /dev/sdb1 /mnt"
alias pacsearch="pacman -Qn"
alias pacclean="sudo pacman -Rns $(pacman -Qtdq)"
alias pacupdate="sudo pacman -Syu"

alias cheat="cht.sh $*"
alias cheats="cheat --shell $*"

# for my second monitor
alias monitor="~/.local/bin/xrandr-DVI_D"
alias xprofile="~/.local/bin/xprofile"

alias rtfml=rtfml
alias rtfm=rtfm
alias mcd=mcd
alias cdl=cdl
alias backup=backup
alias gfind=gfind
alias lfind=lfind
alias cht.sh="~/.local/bin/cht.sh"      #https://github.com/chubin/cheat.sh
alias arjun="~/.local/bin/arjun"

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
lfind() { find . -iname $@ 2>/dev/null; }

mcd() { mkdir -p $1; cd $1; }
cdl() { cd $1; l; }
rtfm() { help $@ || man $@ || ? $@; }
rtfml() { man -k $@; }
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
    ##xtrem-color|*256color) color_prompt=yes;
    screen-|*256color) color_prompt=yes;
esac

# --- Some builtin path ---
PATH="$HOME/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

# --- EDITOR ---
export EDITOR=nvim
export VISUAL=nvim
export EDITOR_PREFIX=nvim

# -- Init xrandr ---
#xrandr --output VGA-1-2 --left-of VGA-1 --auto
#xrandr --output VGA-1-1 --auto --output DVI-I-0 --left-of VGA-1-1
#~/.local/bin/xprofile

export BAT_THEME="Sublime Snazzy"

#--- cheat.sh ---
. ~/.bash.d/cht.sh

##########################################################
# LANGUAGES
##########################################################

# --- GO ----
export GOROOT=/usr/local/go
export PATH=$PATH:$GOROOT/bin

export GOPATH=$HOME/golib
export PATH=$PATH:$GOPATH/bin

# --- RUST ---
export PATH="$HOME/.cargo/bin${PATH:+:${PATH}}"

# --- NPM | NVM ---
source /usr/share/nvm/init-nvm.sh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# --- PYTHON ---
export PATH=/Library/Frameworks/Python.framework/Versions/3.9/bin$PATH

# --- RUBY ---
# export PATH="${PATH}:$(ruby -rubygems -e "puts Gem.user_dir")/bin:/home/user/.composer/vendor/bin"
export GEM_HOME="$(ruby -e 'puts Gem.user_dir')"
export PATH="$PATH:$GEM_HOME/bin"

 #--- cd helpers ---
. $HOME/.config/z/z.sh
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"

PATH="/home/daun/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/daun/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/daun/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/daun/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/daun/perl5"; export PERL_MM_OPT;

