source $PREFIX/etc/fish/config.fish

#alias g="git "
alias pkgu="pkg update"
alias pkgup="pkg upgrade"
alias pkgc="pkg autoclean"
alias pkgls="pkg list-installed"
alias pkgs="pkg search "
alias pkgf="pkg files "
alias pkgd="pkg show "
alias pkgi="pkg install "
alias pkgrm="pkg uninstall "
alias pkglsu="apt list --upgradable"
#alias xfce="termux-x11 :1 -xstartup \"dbus-launch --exit-with-session xfce4-session\""

function fish_prompt
    printf '%s > ' (basename (pwd))
end