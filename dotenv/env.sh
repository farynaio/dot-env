#!/bin/sh

source $HOME/.dotenv/exports.sh
source $HOME/.dotenv/aliases.sh

if command -v 'docker-machine' 2>/dev/null; then
  eval "$(docker-machine env default &>/dev/null)"
fi

# source $(brew --prefix php-version)/php-version.sh && php-version 5

checkport() {
  if [[ -z $1 ]]; then
    echo "No port specified!"
    return 1
  fi

  lsof -nP -iTCP:$1 | grep LISTEN
}

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
