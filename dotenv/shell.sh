if hash docker-machine 2> /dev/null; then
  eval "$(docker-machine env default &>/dev/null)"
fi

# source $(brew --prefix php-version)/php-version.sh && php-version 5


if [[ -z $USER_NAME ]]; then
  echo "No USER_NAME provided!"
else
  git config --global user.name "$USER_NAME"
fi

if [[ -z $EMAIL ]]; then
  echo "No EMAIL provided!"
else
  git config --global user.email "$EMAIL"
fi

if [[ -n $SIGNING_KEY ]]; then
  git config --global user.signingkey "$SIGNING_KEY"
fi


docker-ip() {
  # docker inspect --format '{{ .NetworkSettings.IPAddress }}' "$@"
  docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$@"
}

db-devel-backup() {
  local FILE=${1:-"backup.sql"}
  local CONTAINER=wordpress_db
  local DATABASE=wordpress
  local USER=wordpress
  local PASSWORD=wordpress
  local PREFIX=wp_

  echo "Database back up started..."

  docker exec $CONTAINER /usr/bin/mysqldump --opt -Q -u $USER --password=$PASSWORD $DATABASE ${PREFIX}options ${PREFIX}posts ${PREFIX}postmeta ${PREFIX}terms ${PREFIX}term_taxonomy ${PREFIX}term_relationships ${PREFIX}termmeta > $FILE

  echo "Database backing up finished."
}

db-devel-restore() {
  local FILE=${1:-"backup.sql"}
  local CONTAINER=wordpress_db
  local DATABASE=wordpress
  local USER=wordpress
  local PASSWORD=wordpress

  if [[ -e $FILE  ]]; then
    echo "Restoring database..."
    cat $FILE | docker exec -i $CONTAINER /usr/bin/mysql -u $USER --password=$PASSWORD $DATABASE
    echo "Database restored."
  else
    echo "File $FILE doesn't exists."
  fi
}

is_darwin() {
  [[ $(uname -s) == 'Darwin' ]] && result=true || result=false
  echo $result
}

is_linux() {
  [[ $(uname -s) == 'Linux' ]] && result=true || result=false
  echo $result
}

checkport() {
  if [[ -z $1 ]]; then
    echo "No port specified!"
    return 1
  fi

  netstat -ltnp | grep -w ':$1'
  # lsof -nP -iTCP:$1 | grep LISTEN
}

command_exists() {
  [ -x "$(command -v $1)" ] && result=true || result=false
  echo $result
}


export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"

export ANDROID_HOME="$HOME/Development/Java/sdk-android"
export ANDROID_SDK_ROOT="$HOME/Development/Java/sdk-android"

if [[ -e /usr/libexec/java_home ]]; then
  export JAVA_HOME=$(/usr/libexec/java_home)
fi

export EDITOR="/usr/bin/vim"
export SED=`which sed`
export MU_HOME="~/Maildir"

export NPM_PACKAGES="$HOME/.npm-packages"
export NODE_PATH="$NPM_PACKAGES/bin"
export NODE_PATH=$NODE_PATH:$NPM_PACKAGES/lib/node_modules
export LEDGER_FILE="$HOME/Dropbox/emacs/ledger/private.ledger.gpg"

if [[ -e $HOME/bin ]]; then
  export PATH="$HOME/bin:$PATH"
fi

export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.composer/vendor/bin:$PATH"
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
export PATH="/usr/local/share/android-sdk/tools/bin:$PATH"
export PATH="/usr/local/mysql/bin:/usr/local/mysql/bin/mysqladmin:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.npm-packages/bin:$PATH"
export PATH="$NPM_PACKAGES/bin:$PATH"
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
export PATH="/usr/local/opt/openssl/bin:$PATH"
export PATH="$HOME/Dropbox/devel/scripts:$PATH"
export PATH="$PATH:/usr/local/opt/python/libexec/bin"
export PATH="$PATH:~/Library/Python/3.7/bin"
export PATH="/usr/local/opt/node@10/bin:$PATH"

if hash rbenv &> /dev/null; then
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

alias z="zsh"
alias ports="netstat -tulpn"
alias ports_opened='netstat -pln'
alias rm='rm -i '
alias cp='cp -i '
alias mv='mv -i '

if $(is_darwin); then
  alias ssh_check_connections="lsof -i -n | egrep '\<ssh\>'"
  alias ssh_kill_connections="kill -9 $(lsof -i -n | egrep '\<ssh\>'|tr -s ' '|cut -d' ' -f2)"
  alias emacs_find_package='find /Applications/Emacs.app -regex '
  alias hosts_clean="sudo dscacheutil -flushcache"
  alias virtualenvwrapper='source /usr/local/bin/virtualenvwrapper.sh'
  alias ls='ls -G '
fi

if $(is_linux); then
  alias iptables_reload="iptables -F; iptables-restore < /etc/iptables/rules.v4"
  alias apt_installed='apt list --installed '
  alias services_status='service --status-all'
  alias services_enabled='systemctl list-unit-files | grep enabled'
  alias ls='ls --color '
fi

if $(command_exists "mysqld_safe"); then
  alias start_mysql='mysqld_safe &'
  alias stop_mysql='mysqladmin shutdown'
fi

if $(command_exists "rbenv"); then
  alias taskjuggler='$HOME/.rbenv/versions/`rbenv global`/bin/tj3 '
fi

if $(command_exists "vim"); then
  alias vi="vim "
fi

if $(command_exists "mc"); then
  alias mc=". /usr/local/Cellar/midnight-commander/4.8.22/libexec/mc/mc-wrapper.sh"
fi

if $(command_exists "zsh"); then
  alias f="/usr/local/bin/zsh "
  alias z="zsh "
fi

if $(command_exists "git"); then
  alias g="git "
fi

if $(command_exists "yarn"); then
  alias y="yarn "
fi

if $(command_exists "tmux"); then
  alias ta="TERM=screen-256color-bce tmux -2 attach || TERM=screen-256color-bce tmux new -s "
  alias tnew="TERM=screen-256color-bce tmux -2 new -s "
  alias tls="tmux ls "
  alias tkill="tmux kill-session -t "
fi

if $(command_exists "wget"); then
  alias download_page="wget -E -H -k -K --random-wait -U 'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1.6) Gecko/20070802 SeaMonkey/1.1.4' -e robots=off -p "
fi

if $(command_exists "mbsync"); then
  alias offlineimap="mbsync -a && mu index"
fi

if $(command_exists "ledger"); then
  if [[ $(command_exists "gpg") ]]; then
    alias eledger="gpg --batch -d -q $LEDGER_FILE | ledger -f - "
  fi
fi
