#!/bin/bash
DOTFILES=`pwd`

# Make sure vim, git, zsh, tmux, curl, etc are installed
# Maybe install homebrew if you're on a mac


# SETUP ZSH

## Install oh-my-zsh
git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh

## Link our zsh dotfile
ln -s $DOTFILES/zshrc ~/.zshrc

## TODO read zsh plugin list (gulp) and add to ~/.oh-my-zsh/custom

## Make zsh default shell
chsh -s /bin/zsh


# SETUP VIM

## Install pathogen and make tmp/backup/undo dirs
mkdir -p ~/.vim/autoload ~/.vim/bundle ~/.vim/tmp ~/.vim/backup ~/.vim/undo && \
  curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

## Install plugins
cd ~/.vim/bundle/
while read -r line
do
  plug=$line
  git clone git://github.com/$plug.git 
done < $DOTFILES/vimplugins

## Link our vim dotfile
ln -s $DOTFILES/vimrc ~/.vimrc


# SETUP TMUX

## Link our tmux config
ln -s $DOTFILES/tmux.conf ~/.tmux.conf
