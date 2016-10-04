# This creates the LESSOPEN variable, and sets up less
eval `lessfile`

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

export LESS='-caeisMR'

# I prefer ls to sort upcase files before lowercase
export LC_COLLATE=C

# let dbus and systemd know these
#dbus-update-activation-environment --systemd  DBUS_SESSION_BUS_ADDRESS XAUTHORITY DISPLAY PATH

# Start the gpg-agent if not already running
if ! pgrep -x -u "${USER}" gpg-agent >/dev/null 2>&1; then
  gpg-connect-agent /bye >/dev/null 2>&1
fi
