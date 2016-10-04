#!/bin/sh

userresources=$HOME/.Xresources

# merge in defaults 

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

# setup keymap modifiers for Emacs (with caps-lock -> HYPER modifier)
xmodmap ~/.Xmodmap
# so I don't have to press the back-tick twice
xmodmap -e "keycode  49 = grave notsign dead_grave brokenbar"
# Enable zapping (C-A-<Bksp> kills X)
setxkbmap -option terminate:ctrl_alt_bksp

xsetroot -cursor_name left_ptr

# sensible defaults
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
	[ -x "$f" ] && . "$f"
    done
    unset f
fi

# set keyboard rate
xset r rate 350 60

# set screen brightness
xrandr --output LVDS --brightness 1.2

# start compton and background it
/usr/bin/compton -b

# Enforce correct locales from the beginning:
# LC_ALL is unset since it overwrites everything
# LANG=en_GB.UTF-8 is used, except for:
# LC_MESSAGES=C never translates program output

unset LC_ALL
export LANG=en_GB.UTF-8
export LC_MESSAGES=C
export XAUTHORITY=/home/neil/.Xauthority

# Use XToolkit in java applications
export AWT_TOOLKIT=XToolkit

# restore background image
/home/neil/.fehbg

# Enable core dumps in case something goes wrong
ulimit -c unlimited

# Start window manager like this, e.g.:
# % WM=awesome startx
# if WM isn't defined in environment, default to xmonad.
[[ -z "$WM" ]] && WM="xmonad"
  
logmsg () 
{
    echo ">>>>>> Starting $WM on $(date)"
}
	
case $WM in
    awesome)
	logmsg
	exec /usr/bin/awesome
	;;
    i3)
	logmsg
	exec /usr/bin/i3 -V 
	;;
    ratpoison)
	logmsg
	exec /usr/bin/ratpoison
	;;
    spectrwm)
	logmsg
	exec /usr/bin/spectrwm 
	;;
    xmonad)
	logmsg
	PATH=$PATH:$HOME/.xmonad/bin
#	tray &
	exec /usr/bin/xmonad 
	;;
    *)
	logmsg
	# could put xterm here...
	exec /usr/bin/twm
	;;
esac
