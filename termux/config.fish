source $PREFIX/etc/fish/config.fish

export PATH="$HOME/bin:$PATH"
export EDITOR="vim"
export VISUAL="vim"

# Disable Next.js telemetry
export NEXT_TELEMETRY_DISABLED=1

if status is-interactive; and test -n $EAT_SHELL_INTEGRATION_DIR
  source $EAT_SHELL_INTEGRATION_DIR/fish
end

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
alias pkglsi="pkgls | grep '\[installed\]'"
alias pkgs="pkg search "
alias pkgf="pkg files "
alias pkgd="pkg show "
alias pkgi="pkg install "
alias pkgrm="pkg uninstall "
alias pkglsu="apt list --upgradable"
alias g="git"
alias e="my_emacs_run"

# list ssh tunnels
alias ssh-ls="ps -ef | grep '[s]sh'"

# Delete all emails market by notmuch with deleted tag
alias notmuch-rm="notmuch search --format=text0 --output=files tag:deleted | xargs -0 --no-run-if-empty rm;notmuch new"

# Strip isync IDs from all files in current folder
alias isync-strip="find . -name '*U=*:*' -exec bash -c 'mv \"\$1\" \"\${1%,U=*}\"' _ {} \;"

function fish_prompt
    printf '%s > ' (basename (pwd))
end