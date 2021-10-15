# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

# info : http://ss64.com/bash/syntax-prompt.html
if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm*|rxvt*)
        PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
        ;;
    *)
        ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='exa -al --color=always --group-directories-first'
    #alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF --group-directories-first'
alias la='ls -A --group-directories-first'
alias l='ls -lF --group-directories-first'
alias lR='ll -alfhN --color=auto --group-directories-first | less '

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi



# mk 18-Jul-2016

LC_COLLATE="C"

# info : http://ss64.com/bash/syntax-prompt.html
# Reset
Color_Off='\033[00m'      # Text Reset

# Regular Colors
Black='\033[0;30m'        # Black
Red='\033[0;31m'          # Red
Green='\033[0;32m'        # Green
Yellow='\033[0;33m'       # Yellow
Blue='\033[0;34m'         # Blue
Purple='\033[0;35m'       # Purple
Cyan='\033[0;36m'         # Cyan
White='\033[0;37m'        # White

# Bold
BBlack='\033[1;30m'       # Black
BRed='\033[1;31m'         # Red
BGreen='\033[1;32m'       # Green
BYellow='\033[1;33m'      # Yellow
BBlue='\033[1;34m'        # Blue
BPurple='\033[1;35m'      # Purple
BCyan='\033[1;36m'        # Cyan
BWhite='\033[1;37m'       # White

# Underline
UBlack='\033[4;30m'       # Black
URed='\033[4;31m'         # Red
UGreen='\033[4;32m'       # Green
UYellow='\033[4;33m'      # Yellow
UBlue='\033[4;34m'        # Blue
UPurple='\033[4;35m'      # Purple
UCyan='\033[4;36m'        # Cyan
UWhite='\033[4;37m'       # White

# Background
On_Black='\033[40m'       # Black
On_Red='\033[41m'         # Red
On_Green='\033[42m'       # Green
On_Yellow='\033[43m'      # Yellow
On_Blue='\033[44m'        # Blue
On_Purple='\033[45m'      # Purple
On_Cyan='\033[46m'        # Cyan
On_White='\033[47m'       # White

# High Intensty
IBlack='\033[0;90m'       # Black
IRed='\033[0;91m'         # Red
IGreen='\033[0;92m'       # Green
IYellow='\033[0;93m'      # Yellow
IBlue='\033[0;94m'        # Blue
IPurple='\033[0;95m'      # Purple
ICyan='\033[0;96m'        # Cyan
IWhite='\033[0;97m'       # White

# Bold High Intensty
BIBlack='\033[1;90m'      # Black
BIRed='\033[1;91m'        # Red
BIGreen='\033[1;92m'      # Green
BIYellow='\033[1;93m'     # Yellow
BIBlue='\033[1;94m'       # Blue
BIPurple='\033[1;95m'     # Purple
BICyan='\033[1;96m'       # Cyan
BIWhite='\033[1;97m'      # White

# High Intensty backgrounds
On_IBlack='\033[0;100m'   # Black
On_IRed='\033[0;101m'     # Red
On_IGreen='\033[0;102m'   # Green
On_IYellow='\033[0;103m'  # Yellow
On_IBlue='\033[0;104m'    # Blue
On_IPurple='\033[10;95m'  # Purple
On_ICyan='\033[0;106m'    # Cyan
On_IWhite='\033[0;107m'   # White

PS1="\[$Cyan\][\t]\[$Color_Off\] \[$BCyan\]\u\[$Color_Off\] => \w\[$Color_Off\]\n> \[$Color_Off\]"
PS2="\[$Cyan\][\t]\[$Color_Off\] \[$BCyan\]\u\[$Color_Off\] => \w\[$Color_Off\]\n# \[$Color_Off\]"
PROMPT="\[$Cyan\][\t]\[$Color_Off\] \[$BCyan\]\u\[$Color_Off\] => \w\[$Color_Off\]\n> \[$Color_Off\]"

# set terminal title (including for the tabs) because stupid GNOME terminal removed the damn feature
# set-title Foo
ORIG_PS1=$PS1
set-title(){
TITLE="\e]2;$@\a"
PS1="${ORIG_PS1}\[${TITLE}\]"
}

# mk 06-Feb-2018
export GOROOT=/usr/local/go
export PATH=$PATH:/usr/local/go/bin
export GOPATH=$HOME/go
export PATH=$PATH:/$GOPATH/bin

start_agent () {
    #echo "initializing new SSH agent...----+++----...."
    # spawn ssh-agent
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
    #echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add
}

if [ -f "${SSH_ENV}" ]; then
    . "${SSH_ENV}" > /dev/null
    ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        start_agent;
    }
else
    start_agent;
fi

# rust config
#export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.cargo/bin${PATH:+:${PATH}}"

# Use some built-in bash
PATH="$HOME/bin:$PATH"

# FIXME PATH GO not included
# Should all PATH in one area but I have much more paths.
export PATH=$PATH:/usr/local/go/bin:/home/$HOME/go/bin

#Sen Mar 16 02:33:54 WIB 2020
############################# Edit Shortcuts #############################

export EDITOR=vi
export VISUAL=vi
export EDITOR_PREFIX=vi

export VIMSPELL=(~/.vim/spell/*.add)
declare personalspell=(~/.vimpersonal/spell/*.add)
[[ -n "$personalspell" ]] && VIMSPELL=$personalspell
declare privatespell=(~/.vimprivate/spell/*.add)
[[ -n $privatespell ]] && VIMSPELL=$privatespell

# Commonly edited files.

declare -A edits=(
[bashrc]=~/.bashrc
[personal]=~/.bash_personal
[private]=~/.bash_private
[profile]=~/.profile
[spell]=$VIMSPELL
)

for cmd in "${!edits[@]}"; do
    path=${edits[$cmd]}
    case $PLATFORM in
        *) alias $EDITOR_PREFIX$cmd="$EDITOR '$path';warnln 'Make sure you git commit your changes (if needed).'" ;;
    esac
done

############################# Lynx with duck-duck-go  #############################

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
alias "?"=duck

############################# alias helper  #############################
#Sen Mar 16 22:55:50 WIB 2020
alias df='df -h'
alias '??'='trans :id'
alias reload='source ~/.bashrc'

alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

alias path='echo -e ${PATH//:/\\n}'

alias untar='tar -xvzf'

# prevent mv and cp from overriding
alias mv='mv -i'
alias cp='cp -i'


############################# Directory shortcut  #############################
# Directory (cd) shortcuts. Using associative arrays pointing to
# environment variable names cuz names might not always match (otherwise
# would just ${foo^^} them). Might want to use a different env variable
# name at some point.

declare -A directories=(
[repos]=REPOS
[config]=CONFIG
[personal]=PERSONAL
[private]=PRIVATE
[tokens]=TOKENS
[downloads]=DOWNLOADS
[desktop]=DESKTOP
[pictures]=PICTURES
[videos]=VIDEOS
[images]=DISKIMAGES
[vmachines]=VMACHINES
[readme]=README
[documents]=DOCUMENTS
[project]=PROJECT
)

for k in "${!directories[@]}"; do
    v=${directories[$k]}
    alias $k='\
        if [[ -n "$'$v'" ]];\
        then cd "$'$v'";\
        else\
            warnln "\`\$'$v'\` not set. Consider adding to ~/.bash_{personal,private}.";\
            fi'
        done

# Detect reasonable defaults (override in .bash_private). You'll want to
# set CONFIG in your PERSONAL or PRIVATE locations. See the following for
# examples of how to do this:
#
#    https://github.com/<user-name>/config-personal-sample
#    https://github.com/<user-name>/config-private-sample

declare -A defaults=(
[DOWNLOADS]=~/Downloads
[REPOS]=~/Repos
[DESKTOP]=~/Desktop
[DOCUMENTS]=~/Documents
[README]=~/Documents/README   # README WorldPress content
[PICTURES]=~/Pictures
[VIDEOS]=~/Videos
[DISKIMAGES]=~/DiskImages     # linux, arch, raspberrian, etc.
[VMACHINES]=~/VMachines       # vmware, virtual box, etc.
[TRASH]=~/Trash               # trash (see trash)
[PROJECT]=~/Project
)

for k in "${!defaults[@]}"; do
    v=${defaults[$k]}
    export $k=$v
done

# FIXME the sites not show up.
#declare -A sites=(
#	[github]=github.com
#	[gitlab]=gitlab.com
#	[protonmail]=protonmail.com
#	[skilstak]=skilstak.io
#	[dockhub]=hub.docker.com
#	[twitter]=twitter.com
#	[medium]=medium.com
#	[reddit]=reddit.com
#	[patreon]=patreon.com
#	[paypal]=paypal.com
#	[hackerone]=hackerone.com
#	[youtube]=youtube.com
#	[bugcrowd]=bugcrowd.com
#	[synack]=synack.com
#	[bls]=bls.gov
#	[twitch]=twitch.com
#	[vimeo]=vimeo.com
#	[emojipedia]=emojipedia.com
#	[netflix]=netflix.com
#	[amazon]=amazon.com
#)
#
#for shortcut in "${!sites[@]}"; do
#	url=${sites[$shortcut]}
#	alias $shortcut="open https://$url &>/dev/null"
#done

################### Simple Todo Utility Using Markdown ###################

todo () {
    #[[-z is Test string is non-empty
    if [[ -z "${TODOFILE}" ]] ; then
        echo 'var is unset'
    else
        echo "var is set to '$TODOFILE'";
    fi
    #[[-n is Test string is empty
    if [[ -n "$TODOFILE" ]]; then
        # FIXME $TODOFILE not detect
        $EDITOR $TODOFILE
    else
        telln 'No `$TODOFILE` set.'
    fi
} && export -f todo

################### temporary ###################
setPrompt()
{  # {{{
    local TITLEBAR=''
    local HOST_NAME='\h'

    case ${TERM} in
        xterm*)
            TITLEBAR="\[\033]0;[\$(lastcmd)] (\$?) \w\007\]"
            ;;
    esac

    [[ -s /etc/HOSTNAME ]] && HOST_NAME=$(</etc/HOSTNAME)

    if [[ "x" != "x${GIT}" ]] ; then
        export PS1="${TITLEBAR}\[\033[40m\]\[\033[33m\]${HOST_NAME/.*}\[\033[1;33m\][\t]\[\033[1;34m\](\$?)\[\033[1;35m\]\$(git_branch)\[\033[1;36m\]\w\[\033[31m\]\\$\\[\033[0m\] "
    else
        export PS1="${TITLEBAR}\[\033[40m\]\[\033[33m\]${HOST_NAME/.*}\[\033[1;33m\][\t]\[\033[1;34m\](\$?)\[\033[1;35m\]\[\033[1;36m\]\w\[\033[31m\]\\$\\[\033[0m\] "
    fi

        # "Ultimate" Bash debugging prompt, from
        # http://wiki.bash-hackers.org/scripting/debuggingtips
        # (also, ${BASH_LINENO[*]} has linenumbers in call stack
        export PS4='+(${BASH_SOURCE}:${LINENO}): ${FUNCNAME[0]:+${FUNCNAME[0]}(): }'

        unset -f setPrompt
    }  # setPrompt()  }}}

PATH="$HOME/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="$HOME/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"$HOME/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"; export PERL_MM_OPT;

#Sab Jun 13 05:26:20 WIB 2020
# --- Tracks your most used directories, based on 'frecency' ---
. ~/Repos/z/z.sh
