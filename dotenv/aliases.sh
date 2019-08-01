#!/bin/sh

alias ls="ls -G "
alias ports="netstat -tulpn"

if $(is_darwin); then
  alias ssh_check_connections="lsof -i -n | egrep '\<ssh\>'"
  alias ssh_kill_connections="kill -9 $(lsof -i -n | egrep '\<ssh\>'|tr -s ' '|cut -d' ' -f2)"
  alias emacs_find_package='find /Applications/Emacs.app -regex '
  alias hosts_clean="sudo dscacheutil -flushcache"
  alias virtualenvwrapper='source /usr/local/bin/virtualenvwrapper.sh'
fi

if $(is_linux); then
  alias iptables_reload="iptables-restore < /etc/iptables/rules.v4"
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
