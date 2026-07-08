#!/bin/bash

if tmux has-session -t mysession 2>/dev/null; then
  tmux attach -t mysession
else
 tmux new-session -d -s mysession -n emacs fish
 tmux new-window -t mysession -n fish fish
 tmux attach -t mysession

  # tmux new-session -d -s mysession -n emacs 'fish' \; new-window -t mysession -n sh 'fish' \; select-window -t mysession:1 attach -t mysession
fi