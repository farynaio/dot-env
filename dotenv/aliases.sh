#!/bin/sh

alias ssh_home_server="ssh -l serwer1472142 serwer1472142.home.pl"
alias ssh_check_connections="lsof -i -n | egrep '\<ssh\>'"
alias ssh_kill_connections="kill -9 $(lsof -i -n | egrep '\<ssh\>'|tr -s ' '|cut -d' ' -f2)"
alias ssh_zenbox='ssh -l adamfary s13.zenbox.pl'
alias virtualenvwrapper='source /usr/local/bin/virtualenvwrapper.sh'
alias start_mysql='mysqld_safe &'
alias stop_mysql='mysqladmin shutdown'
alias emacs_find_package='find /Applications/Emacs.app -regex '

alias vi="vim "
alias ls="ls -G "
alias mc=". /usr/local/Cellar/midnight-commander/4.8.20/libexec/mc/mc-wrapper.sh"
alias f="/usr/local/bin/zsh "
alias g="git "
alias y="yarn "
# alias f="fish -l"
# alias subl="mvim +NERDTree "
# alias tmux="TERM=screen-256color-bce tmux -2 a || tmux new "
alias ta="TERM=screen-256color-bce tmux -2 attach || TERM=screen-256color-bce tmux new -s "
alias tnew="TERM=screen-256color-bce tmux -2 new -s "
alias tls="tmux ls "
alias tkill="tmux kill-session -t "
