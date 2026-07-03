#!/bin/bash

tmux new-session -d -s mysession -n xfce '~/bin/xfce-start.sh' \; \
  new-window -t mysession -n emacs 'emacs' \; \
  attach -t mysession

exec tmux attach -t mysession
