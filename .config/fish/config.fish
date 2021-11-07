if status is-interactive
end

# Commands to run in interactive sessions can go here

set fish_greeting ""

#set -gx TERM xterm-256color
set -gx TERM screen-256color
set -gx EDITOR nvim

# use vi mode in fish shell
fish_vi_key_bindings

## --- powerline setup ---
## we use 'tide' instead in fish so we do not need powerline
#set -gx fish_function_path $fish_function_path "/usr/share/powerline/bindings/fish/powerline-setup.fish"
#source /usr/share/powerline/bindings/fish/powerline-setup.fish
#powerline-setup

# abbreviation
abbr c "clear"
abbr v "nvim"
abbr vi "nvim"
abbr nv "nvim"
##abbr trans "trans :id"
abbr rm "rm -rf"
abbr cp "cp -i"
abbr mv "mv -i"

## -- docker abbrev --
abbr "docker-ps" "docker ps -a"
abbr "docker-img" "docker images --all"


# aliases
## --- ls stuff ---
alias ls "exa -h --color=always"
alias ll "ls -alF --color=always --group-directories-first"
alias l "ls -lF --color=always --group-directories-first"

alias cat "bat $1"

## --- grep stuff ---
#alias grep "rg --colors 'match:fg:magenta' --colors 'line:bg:yellow'"
alias fgrep "fgrep --color=always"
alias egrep "egrep --color=always"
alias igrep "grep -i --color=always"
alias rgrep "igrep -R --color=always"

## --- helper stuff ---
alias myip "ip a | grep "inet" | igrep -Fv 127.0.0.1 | awk '{print $2}'"
alias pacsearch "pacman -Qn"
alias pacupdate "sudo pacman -Syu"
alias pacclean "sudo pacman -Rns (pacman -Qtdq)"
alias pacremove "sudo pacman -Rns $argv"

## -- docker stuff --
#alias "docker-ps" "docker ps -a"
#alias "docker-img" "docker images --all"

## -- config files --
alias fishrc "nvim ~/.config/fish/config.fish"
alias bashrc "nvim ~/.bashrc"
alias tmuxrc "nvim ~/.tmux.conf"
alias nvimrc "nvim ~/.config/nvim/init.lua"
alias awesomerc "nvim ~/.config/awesome/rc.lua"

# --- Git aliases ---
# alias gl="git log --pretty=oneline --abbrev-commit"
alias gl "git log --graph --pretty=format:'%C(yellow)%h%Creset -%Cred%d%Creset %s %Cgreen| %cr %C(bold blue)| %an%Creset' --abbrev-commit --date=relative"
alias gs "git status --branch --short --untracked-file=all --ahead-behind"
alias gcm "git commit"

alias rl "source ~/.config/fish/config.fish"

alias cheat "cht.sh $argv"
alias cheats "cht.sh --shell $argv"

#set -gx PATH bin $PATH
#set -gx PATH ~/.local/bin $PATH

## -- function calls --

function duck -d "open duckduckgo with lynx"
    lynx "https://duckduckgo.com/lite?q=$argv"
end

function tr-id -d "translate into bahasa"
    trans :id
end

function tr-en -d "translate bahasa into english"
    trans id:en
end
