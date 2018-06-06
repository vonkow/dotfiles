#!/bin/bash

# TODO update this a bunch and make betterer

# Homebrew
# TODO if is osx only
#/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Oh My zsh
# TODO use wget if not osx
#sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# Emacs
#brew install emacs --with-cocoa

# AG
brew install ag

# Fonts
## Plex
git clone https://github.com/IBM/plex.git
find plex -name '*.ttf' -exec cp {} /Library/Fonts/ \;
rm -rf plex

# JS: nvm and node and yarn
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.9/install.sh | bash
nvm install node
brew install yarn --without-node

## work things
yarn global add flow-bin
yarn global add prettier
yarn global add eslint eslint-config-airbnb eslint-plugin-import eslint-plugin-jasmine eslint-plugin-jest eslint-plugin-jsx-a11y eslint-plugin-react eslint-plugin-html babel-eslint
yarn global add jest

## js deps
yarn global add tern js-beautify jshint

# PY

## 3
brew install python

## deps
pip install --upgrade "jedi>=0.9.0" "json-rpc>1.8.1" "service_factory>=0.1.5"
pip install flake8
# pip install autoflake  # if we want to cull unused deps
