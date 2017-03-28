#!/bin/bash

# js deps
npm install -g tern js-beautify jshint

# py deps
pip install --upgrade "jedi>=0.9.0" "json-rpc>1.8.1" "service_factory>=0.1.5"
pip install flake8
# pip install autoflake  # if we want to cull unused deps
