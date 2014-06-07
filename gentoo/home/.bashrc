# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive, exit
	return
fi

complete -d cd
shopt -s cdspell

# [ -r ~/.prompt ] && . ~/.prompt 

export MANPATH="$MANPATH:/data/essentials/man"
export PATH="/home/alvatar/.bin:$PATH"

# source .bin/bashmarks.sh

LESS="-i $LESS"

#Old config included this    hl=05;44;37:
LS_COLORS="rs=0:fi=0:di=01;34:ex=0;04:ln=0;04;36:pi=41;33:so=01;41:do=01;05;35:bd=41;33;01:cd=41;33;01:or=01;05;31;41:mi=01;05;37;41:ca=05;30;41:tw=36;04:ow=01;36:st=01;04"
PS1="\[\033[01;01m\]\u@\h\[\033[01;0;01m\]\w \$\[\033[00m\] "

shopt -s cdable_vars
shopt -s histappend

# export ANDROID_NDK_DIR=/data/sources/android/android-ndk-r8
# export ANDROID_SDK_DIR=/data/sources/android/android-sdk-linux
# export PATH=$PATH:/usr/local/Gambit-C/bin
# export PATH=$PATH:$ANDROID_SDK_DIR/tools
# export PATH=$PATH:$ANDROID_SDK_DIR/platform-tools
# export PATH=$PATH:$ANDROID_NDK_DIR
# export PATH=$PATH:/usr/local/heroku/bin
