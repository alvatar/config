# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how many often would you like to wait before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

DISABLE_CORRECTION="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(gem git git-extras gnu-utils zshmarks)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=/home/alvatar/.bin:/home/alvatar/.bin:/usr/local/bin:/usr/bin:/bin:/opt/bin:/usr/x86_64-pc-linux-gnu/gcc-bin/4.5.4:/usr/games/bin/

export MANPATH="$MANPATH:/data/essentials/man"
export ANDROID_NDK_PATH=/data/android/android-ndk-r9c
export ANDROID_SDK_PATH=/data/android/sdk
export PATH="/home/alvatar/.bin:$PATH"
export PATH=$PATH:/usr/local/Gambit-C/bin
export PATH=$PATH:$ANDROID_SDK_PATH/tools
export PATH=$PATH:$ANDROID_SDK_PATH/platform-tools
export PATH=$PATH:$ANDROID_NDK_PATH
export PATH=$PATH:/usr/local/heroku/bin

export XDG_DESKTOP_DIR=/home/alvatar

# Default heap size of 5mb for Gambit
export GAMBCOPT=m5000

# Don't share history between terminals
setopt no_share_history

alias bower='noglob bower'
