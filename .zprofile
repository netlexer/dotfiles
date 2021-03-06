# This creates the LESSOPEN variable, and sets up less
eval `lessfile`

# Add directories to PATH if they exist
[ -d /usr/lib/surfraw ] && PATH=$PATH:/usr/lib/surfraw
# Add compiler cache *before* rest of path 
[ -d /usr/lib/ccache ] && PATH=/usr/lib/ccache/bin:$PATH
# Append ruby gems, rust, cabal & xmonad bin dirs to path
[ -d $HOME/.gem/ruby/2.3.0/bin ] && PATH=$PATH:$HOME/.gem/ruby/2.3.0/bin
[ -d $HOME/rust/bin ] && PATH=$PATH:$HOME/rust/bin
[ -d $HOME/.cabal/bin ] && PATH=$PATH:$HOME/.cabal/bin
[ -d $HOME/.xmonad/bin ] && PATH=$PATH:$HOME/.xmonad/bin
# Finally, if there's a private user ~/bin directory, prepend it...
[ -d $HOME/bin ] && PATH=$HOME/bin:$PATH

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
