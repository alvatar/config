# XMonad
# .xsession

xsetroot -cursor_name left_ptr

#trayer --edge top --align right --expand true --widthtype request --heighttype pixel --height 20 --transparent true --alpha 256 --SetPartialStrut true --SetDockType true &

setxkbmap es
xmodmap .Xmodmaprc
xbindkeys

xrdb -all .Xdefaults

# Fix an openoffice issue with gtk
#
export SAL_USE_VCLPLUGIN=gen

export EDITOR="vim"

#xset m 2 1

# Display temperature adaptation
xflux -l 40.4 -g -3.6

export VIMHOME="~/.vim"

#xsetroot -solid darkblue
#xloadimage -onroot /data/misc/config/fondo.png
# Workaround For Hardware Accelerated Video Glitching On NVidia
hsetroot -solid "#000000" # Stops The Gray Background
xcompmgr -n &

exec xmonad

