#!/bin/bash

set -o emacs

export USERNAME="Adam Faryna"
export EMAIL="adamfaryna@appdy.net"
export ANDROID_HOME=/usr/local/share/android-sdk
# export ANDROID_HOME=/Users/devil/Development/Java/android-sdk-macosx
export ANDROID_SDK_ROOT=/usr/local/share/android-sdk
#export STUDIO_JDK=/Library/Java/JavaVirtualMachines/1.6.0_45-b06-451.jdk
export JAVA_HOME=$(/usr/libexec/java_home)
export EDITOR="/usr/bin/vim"
# export PATH=/usr/local/bin:/Users/devil/Development/Java/android-sdk-macosx/tools:$PATH
export SED=`which sed`

export PATH="$HOME/.composer/vendor/bin:$PATH"
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# export PATH=/Users/devil/Development/codenameone/codenameone-build-tools/tools/codenameone-readonly/Ports/iOSPort/xmlvm/install/bin:$PATH
# export PATH=/Users/devil/Development/Java/android-sdk-macosx/platform-tools:$PATH
export PATH=/usr/local/share/android-sdk/tools/bin:$PATH
export PATH="/usr/local/mysql/bin:/usr/local/mysql/bin/mysqladmin:$PATH"
export PATH="/Users/devil/bin:$PATH"
export PATH="$HOME/.npm-packages/bin:$PATH"
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
export PATH="/usr/local/opt/openssl/bin:$PATH"
export PATH="/usr/local/opt/python/libexec/bin:$PATH"
export PATH="~/Dropbox/devel/scripts:$PATH"

export WORKON_HOME=~/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
export DJANGO_SECRET_KEY="@4vjd^wrz-rtvwrnxij6$f^f!%&c2l^6x$u&#t+6&ijk631$8k"

