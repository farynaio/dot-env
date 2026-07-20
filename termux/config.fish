source $PREFIX/etc/fish/config.fish

export PATH="$HOME/bin:$PATH"
export EDITOR="vim"
export VISUAL="vim"

set -gx GPG_TTY (tty)

function my_emacs_run
    if isatty stdin
        eval $EDITOR
    else
        eval $VISUAL
    end
end

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
alias g git
alias e 'my_emacs_run'

function fish_prompt
    printf '%s > ' (basename (pwd))
end