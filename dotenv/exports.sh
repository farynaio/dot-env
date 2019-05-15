#!/bin/sh

export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"

export ANDROID_HOME="/Users/devil/Development/Java/sdk-android"
export ANDROID_SDK_ROOT="/Users/devil/Development/Java/sdk-android"
export JAVA_HOME=$(/usr/libexec/java_home)

export EDITOR="/usr/bin/vim"
# export EDITOR="/usr/loca/bin/emacsclient -c -a emacs"
export VISUAL="/usr/local/bin/emacs"
export SED=`which sed`
export MU_HOME="~/Maildir"

export NPM_PACKAGES="$HOME/.npm-packages"
export NODE_PATH="$NPM_PACKAGES/bin"
export NODE_PATH=$NODE_PATH:$NPM_PACKAGES/lib/node_modules
export LEDGER_FILE="/Users/devil/Dropbox/emacs/ledger/private.ledger.gpg"

if [[ -e $HOME/bin ]]; then
  export PATH="$HOME/bin:$PATH"
fi

export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.composer/vendor/bin:$PATH"
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
export PATH="/usr/local/share/android-sdk/tools/bin:$PATH"
export PATH="/usr/local/mysql/bin:/usr/local/mysql/bin/mysqladmin:$PATH"
export PATH="/Users/devil/bin:$PATH"
export PATH="$HOME/.npm-packages/bin:$PATH"
export PATH="$NPM_PACKAGES/bin:$PATH"
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
export PATH="/usr/local/opt/openssl/bin:$PATH"
export PATH="$HOME/Dropbox/devel/scripts:$PATH"
export PATH="$PATH:/usr/local/opt/python/libexec/bin"

if which rbenv > /dev/null; then
  export PATH="$HOME/.rbenv/shims:$PATH"
fi

export GPG_TTY=$(tty)

# export PAGER="/usr/bin/less -isM -F -X +Gg $LESS"
# export MANPAGER=$PAGER
# export GIT_PAGER=$PAGER
# export MANPATH=$NPM_PACKAGES/share/man:$MANPATH

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

if [[ -e ~/.local.sh ]]; then
  source ~/.local.sh
fi
