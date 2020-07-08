##########################################################
#
#---------------------------------------------------------
# Agung Tuanany .bashrc file
#---------------------------------------------------------
# Last Edited       : Wed 08 Jul 2020 08:44:03 AM WIB
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


##########################################################
# PS1
##########################################################
# XXX FIXME: Give colors. I'm still research
PS1='[\u@\h \W]\$ '


##########################################################
# LANGUAGES
##########################################################

# --- GO ----
export GOROOT=/usr/local/go
export PATH=$PATH:/usr/local/go/bin
export GOPATH=$HOME/go
export PATH=$PATH:/$GOPATH/bin

# --- RUST ---
export PATH="$HOME/.cargo/bin${PATH:+:${PATH}}"

# --- NPM | NVM ---
source /usr/share/nvm/init-nvm.sh



# --- cd helpers ---
. $HOME/.config/z/z.sh

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

# --- Browser ---
alias "?"=duck

# --- Transaltor ---
alias "??"="trans :id"

# --- Tired to type source ~/.basrhc ---
alias reload="source ~/.bashrc"

# --- NPM browser-sync ---
alias serve="browse-sync start --serve --files ."

# prevent mv and cp from overriding
alias mv='mv -i'
alias cp='cp -i'

# --- You get bored on terminal really? ---
alias clear="[ $[$RANDOM % 6] = 0 ] && timeout 4 cmatrix || clear"


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



# Run 'nvm use' automatically every time there's
# a .nvmrc file in the directory. Also, revert to default
# version when entering a directory without .nvmrc

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

