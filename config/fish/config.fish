set CONFIGDIR (dirname (status -f))
set -x PATH /usr/local/bin $PATH
set -x PATH /usr/local/sbin $PATH
set -x PATH /usr/local/share/python3 $PATH
set -x PATH /usr/local/share/python $PATH
set -x PATH /usr/local/share/npm/bin $PATH
set -x PATH ~/Code/bin $PATH


. $CONFIGDIR/bundle/z-fish/z.fish
. $CONFIGDIR/bundle/virtualfish/virtual.fish

set fish_greeting
fortune computers
