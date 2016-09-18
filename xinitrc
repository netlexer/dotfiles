#!/bin/sh

userresources=$HOME/.Xresources

# merge in defaults 

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

# Enable zapping (C-A-<Bksp> kills X)
setxkbmap -option terminate:ctrl_alt_bksp

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# Disable DPMS turning off the screen
#xset -dpms
#xset s off

# Disable bell
xset -b

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

# Use XToolkit in java applications
export AWT_TOOLKIT=XToolkit

# Set background color & restore background image
xsetroot -solid "#333333"
/home/neil/.fehbg

# Enable core dumps in case something goes wrong
ulimit -c unlimited

# Start i3 and log to file
#echo ">>>>>> Starting at $(date)" >> ~/.i3/i3.log
#exec /usr/bin/i3 -V  >> ~/.i3/i3.log

# Start window manager (with default session = i3) 
SESSION=${1:-i3} 
# WM is usually the same as the command to start it, but not always,
# and may include args.
case $SESSION in
    awesome		) WM=awesome;;
    i3			) WM="i3 -V";;
    notion		) WM=notion;;
    pekwm		) WM=pekwm;;
    ratpoison		) WM=ratpoison;;
    spectrwm		) WM=spectrwm;;
    *)			) WM=$1;;
esac

# Start the window manager and log to file
echo ">>>>>> Starting $WM on $(date)" >> ~/.log/${SESSION}.log
exec $WM  >> ~/.log/${SESSION}.log

    