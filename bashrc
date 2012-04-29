SELF=$(test -L "$BASH_SOURCE" && readlink -n "$BASH_SOURCE" || echo "$BASH_SOURCE")
BASEDIR=$(dirname "$BASH_SOURCE")"/"$(dirname $SELF)

for file in "$BASEDIR"/bash/*; do
    source $file
done

PS1="\W $ "
HISTFILESIZE=10000
export CLICOLOR=1
