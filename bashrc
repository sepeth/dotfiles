## Set some vars
SELF=$(test -L "$BASH_SOURCE" && readlink -n "$BASH_SOURCE" || echo "$BASH_SOURCE")
BASEDIR=$(dirname "$BASH_SOURCE")"/"$(dirname $SELF)
EDITOR=vim
VISUAL=$EDITOR
export EDITOR VISUAL
export GPG_TTY=$(tty)
export CLICOLOR=1
export RLWRAP_HOME="$BASEDIR"/rlwrap

## History Control
HISTFILESIZE=10000
HISTCONTROL=ignoreboth
HISTIGNORE="?:??"


## Add hostname to PS1 if I am in SSH session
if [[ -z "$SSH_TTY" ]]; then
    PS1="\W $ "
else
    PS1="[\u@\h \W]\$ "
fi


## Aliases
alias ..='cd ..'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -la'
alias lld='ls -ld'
alias dfh='df -H'
alias grep='grep --colour'
alias mkdir='mkdir -p'
alias psg='ps aux |grep -i'
alias sude='sudo -e'
alias serve='python -m SimpleHTTPServer'
alias less='less -FRX'
alias tree='tree -C'
alias trls='tree -C |less -FRX'
alias fnls='find . |less -FRX'

## Git aliases
alias gd='git diff'
alias ga='git add'
alias gst='git status -sb'

## rlwrap aliases
alias sbcl='rlwrap sbcl'
alias sml='rlwrap sml'

## Directory bookmarks
alias m1='alias g1="cd `pwd`"'
alias m2='alias g2="cd `pwd`"'
alias m3='alias g3="cd `pwd`"'
alias m4='alias g4="cd `pwd`"'
alias m5='alias g5="cd `pwd`"'
alias m6='alias g6="cd `pwd`"'
alias m7='alias g7="cd `pwd`"'
alias m8='alias g8="cd `pwd`"'
alias m9='alias g9="cd `pwd`"'
alias mdump='alias|grep -e "alias g[0-9]"|grep -v "alias m" > ~/.bookmarks'
alias lma='alias | grep -e "alias g[0-9]"|grep -v "alias m"|sed "s/alias //"'
touch ~/.bookmarks
source ~/.bookmarks

cf() {
  cd *$1*/
}

md() {
  mkdir -p "$1" && cd "$1"
}

# Ask wikipedia
wp() {
  dig +short txt ${1}.wp.dg.cx
}


## Virtualenv
mkvirtenv() {
  [ "$#" -eq 1 ] &&
    mkdir -p ~/.virtualenvs/$1 &&
    virtualenv ~/.virtualenvs/$1
}

mkvirtenv3() {
  [ "$#" -eq 1 ] &&
    mkdir -p ~/.virtualenvs/$1 &&
    virtualenv -p `which python3` ~/.virtualenvs/$1
}

virtenv() {
  [ "$#" -eq 1 ] &&
    source ~/.virtualenvs/$1/bin/activate
}


## Tools
source "$BASEDIR"/bash/z/z.sh
command -v lesspipe.sh >/dev/null && eval "$(SHELL=/bin/sh lesspipe.sh)"


## Machine specific bashrc
if [[ -f "$HOME/.bashrc_local" ]]; then
    source "$HOME/.bashrc_local"
fi
