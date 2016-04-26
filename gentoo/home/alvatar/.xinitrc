#trayer --edge top --align right --expand true --widthtype request --heighttype pixel --height 20 --transparent true --alpha 256 --SetPartialStrut true --SetDockType true &

#setxkbmap es
#xbindkeys
#xrdb -all .Xdefaults
#xset m 2 1

#xsetroot -solid darkblue
#xloadimage -onroot /data/misc/config/fondo.png
# Workaround For Hardware Accelerated Video Glitching On NVidia
#hsetroot -solid "#000000" # Stops The Gray Background


xmodmap .Xmodmaprc

xsetroot -cursor_name left_ptr
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
[[ -d ~/.tomorrow-Xdefaults ]] && xrdb -merge ~/.tomorrow-Xdefaults/XresourceTomorrow

export EDITOR="vim"
export VIMHOME="~/.vim"

urxvtd --quiet --opendisplay --fork

# Display temperature adaptation
xflux -l 40.4 -g -3.6

# Trackpad
synclient VertScrollDelta=-50
synclient TapButton2=
synclient TapButton3=2

battery-check.sh &
eval `ssh-agent -s`


exec xmonad

