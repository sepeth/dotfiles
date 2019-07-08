# If not running interactively, don't do anything
[[ $- != *i* ]] && return

## Set some vars
SELF=$(test -L "$BASH_SOURCE" && readlink -n "$BASH_SOURCE" || echo "$BASH_SOURCE")
BASEDIR=$(dirname "$BASH_SOURCE")"/"$(dirname $SELF)
EDITOR=vim
VISUAL=$EDITOR
export EDITOR VISUAL
export CLICOLOR=1
export LESS="FRSXi"
export RLWRAP_HOME="$BASEDIR"/rlwrap
export PYTHONSTARTUP="$BASEDIR"/pystartup.py

## History Control
HISTSIZE=20000
HISTCONTROL=ignoreboth
HISTIGNORE="?:??"

shopt -s autocd cdspell dirspell
shopt -s extglob globstar
shopt -s cmdhist histappend
shopt -s no_empty_cmd_completion
shopt -s huponexit checkjobs
shopt -s direxpand


## Aliases
alias ..='cd ..'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -la'
alias lld='ls -ld'
alias cx='chmod +x'
alias dfh='df -H'
alias grep='grep --colour'
alias mkdir='mkdir -p'
alias psg='ps aux |grep -i'
alias sude='sudo -e'
alias serve='python -m SimpleHTTPServer'
alias tree='tree -C'
alias trls='tree -C |less'
alias fnls='find . |less'
alias ag='ag --pager less'
alias pyag='ag --python'
alias jsag='ag --js'
alias k2='kill -2'
alias hcat='pygmentize -g'
alias bc='bc -ql'
alias gdb='gdb -q'
alias suniq='sort |uniq'

## Git aliases
alias gd='git diff'
alias ga='git add'
alias gst='git status --short --branch'

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

alias sqlite3='sqlite3 -column -header'

cf() {
  cd *$1*/
}

md() {
  mkdir -p "$1" && cd "$1"
}

swp() {
  local tmp=$(mktemp -u .XXXXX)
  mv "$1" $tmp
  mv "$2" "$1"
  mv $tmp "$2"
}

vact() {
    source $1/bin/activate
}

extract-audio() {
    local input="$1"
    local output="${input%.*}.mp3"
    ffmpeg -i "$input" -c:a copy -vn "$output"
}

convert2mp3() {
    local input="$1"
    local output=${input%.*}.mp3
    ffmpeg -i "$input" -ab 320k -map_metadata 0 "$output"
}

rm-bom() {
    local input="$1"
    tail -c +4 "$input" | sponge "$input"
}

## [Optional] Tools
source "$BASEDIR"/bash/z/z.sh
command -v lesspipe.sh  >/dev/null && eval "$(SHELL=/bin/sh lesspipe.sh)"
command -v thefuck      >/dev/null && eval "$(thefuck --alias)"
command -v fortune      >/dev/null && fortune
command -v colordiff    >/dev/null && alias diff='colordiff -u'
command -v direnv       >/dev/null && eval "$(direnv hook bash)"

## rlwrap aliases
if command -v rlwrap >/dev/null; then
    alias sbcl='rlwrap sbcl'
    alias sml='rlwrap sml'
    alias clj='rlwrap clj'
    alias ocaml='rlwrap ocaml'
    alias ocamldebug='rlwrap ocamldebug'
fi

## Source machine specific bashrc
if [[ -f "$HOME/.bashrc_local" ]]; then
    source "$HOME/.bashrc_local"
fi

## Add hostname to PS1 if I am in an SSH session
if [[ -z "$SSH_TTY" ]]; then
    PS1="\w $ "
else
    PS1="[\u@\h \w]\$ "
fi
