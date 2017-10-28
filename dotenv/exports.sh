#!/bin/bash

set -o emacs

export USERNAME="Adam Faryna"
export EMAIL="adamfaryna@appdy.net"
export ANDROID_HOME="/usr/local/share/android-sdk"
export ANDROID_SDK_ROOT="/usr/local/share/android-sdk"
export JAVA_HOME=$(/usr/libexec/java_home)
export EDITOR="/usr/bin/vim"
export SED=`which sed`

export PATH="$HOME/.composer/vendor/bin:$PATH"
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
export PATH="/usr/local/share/android-sdk/tools/bin:$PATH"
export PATH="/usr/local/mysql/bin:/usr/local/mysql/bin/mysqladmin:$PATH"
export PATH="/Users/devil/bin:$PATH"
export PATH="$HOME/.npm-packages/bin:$PATH"
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
export PATH="/usr/local/opt/openssl/bin:$PATH"
export PATH="/usr/local/opt/python/libexec/bin:$PATH"
export PATH="$HOME/Dropbox/devel/scripts:$PATH"

export WORKON_HOME=~/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
export DJANGO_SECRET_KEY="@4vjd^wrz-rtvwrnxij6$f^f!%&c2l^6x$u&#t+6&ijk631$8k"

# Colored man pages: http://linuxtidbits.wordpress.com/2009/03/23/less-colors-for-man-pages/
# Less Colors for Man Pages
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;016m\E[48;5;220m'    # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline
