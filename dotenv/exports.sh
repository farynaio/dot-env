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
