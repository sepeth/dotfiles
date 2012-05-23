function is_in_remote() {
    if [[ -z "$SSH_TTY" ]]; then
        return 1
    fi
    return 0
}

# load files in bash/
SELF=$(test -L "$BASH_SOURCE" && readlink -n "$BASH_SOURCE" || echo "$BASH_SOURCE")
BASEDIR=$(dirname "$BASH_SOURCE")"/"$(dirname $SELF)
for file in "$BASEDIR"/bash/*; do
    source $file
done

if is_in_remote; then
    PS1="[\u@\h \W]\$ "
else
    PS1="\W $ "
fi

HISTFILESIZE=10000
export CLICOLOR=1
