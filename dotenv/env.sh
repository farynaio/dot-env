#! /bin/sh

source $HOME/.dotenv/exports.sh
source $HOME/.dotenv/aliases.sh

eval "$(docker-machine env default &>/dev/null)"

source $(brew --prefix php-version)/php-version.sh && php-version 5
