source $PREFIX/etc/fish/config.fish

export PATH="$HOME/bin:$PATH"
export EDITOR="vim"
export VISUAL="vim"

set -gx GPG_TTY (tty)

# Optional: Update GPG agent TTY on every command (fixes tmux pane switching issues)
function update_gpg_tty --on-event fish_preexec
    if test -n "$GPG_TTY"
        gpg-connect-agent UPDATESTARTUPTTY /bye > /dev/null 2>&1
    end
end

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

if set -q PREFIX
  export DISPLAY=:1
 # MESA_LOADER_DRIVER_OVERRIDE=zink
end

function fish_prompt
    printf '%s > ' (basename (pwd))
end