#!/bin/sh

source $HOME/.dotenv/exports.sh
source $HOME/.dotenv/aliases.sh

eval "$(docker-machine env default &>/dev/null)"

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

source $(brew --prefix php-version)/php-version.sh && php-version 5

if [[ -z $USER_NAME ]]; then
  echo "No $USER_NAME provided!"
else
  git config --global user.name "$USER_NAME"
fi

if [[ -z $EMAIL ]]; then
  echo "No $EMAIL provided!"
else
  git config --global user.email "$EMAIL"
fi

if [[ -z $SIGNING_KEY ]]; then
  echo "No $SIGNING_KEY provided!"
else
  git config --global user.signingkey "$SIGNING_KEY"
fi
