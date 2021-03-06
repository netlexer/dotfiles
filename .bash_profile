#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc
[[ -f ~/.bash_aliases ]] && . ~/.bash_aliases


# This creates the LESSOPEN variable, and sets up less
eval `lessfile`
export LESS='-caeisMR'

# set this to emacsclient (todo)
export EDITOR='emacsclient'
export IRCNICK=daedalux

# Add directories to PATH if they exist
[ -d /usr/lib/surfraw ] && PATH=$PATH:/usr/lib/surfraw
# If there's a private user ~/bin directory, add it...
[ -d $HOME/bin ] && PATH=$PATH:$HOME/bin
# Add compiler cache *before* rest of path
[ -d /usr/lib/ccache ] && PATH=/usr/lib/ccache/bin:$PATH
# Append cabal & xmonad bin dirs
[  -d $HOME/.cabal/bin ] && PATH=$PATH:$HOME/.cabal/bin
[  -d $HOME/.xmonad/bin ] && PATH=$PATH:$HOME/.xmonad/bin

# de-duplicate path components, just in case 
PATH=`perl -e 'print join ":", grep {!$h{$_}++} split ":", $ENV{PATH}'`

# a couple of functions

d () 
{ 
    CONF="/etc/dict/colorit.conf";
    if [ -e $CONF ]; then
        /usr/bin/dict "$@" | colorit -c $CONF | less -R -+c -F -X;
    else
        /usr/bin/dict "$@" | less -R -+c -F -X;
    fi
}


gg () 
{ 
    SURFRAW_google_safe="off";
    /usr/lib/surfraw/google "$@" &> /dev/null
}
