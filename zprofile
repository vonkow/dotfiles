export ZSH=$HOME/.oh-my-zsh

# Bunch of oh my zshrc stuff below, comments stripped

ZSH_THEME="miloshadzic"

CASE_SENSITIVE="true"
# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

plugins=(git ssh-agent)

export PATH="/Users/caz_downing-bryant/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:~/bin:/Users/cazdowning-bryant/Library/Python/2.7/bin"

source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8

# offline-imap bs (it needs python 2)
alias offlineimap="python2.7 /usr/local/Cellar/offlineimap/7.2.1/libexec/offlineimap.py"

# Node cares about this, I guess.
# Way to be lame, node
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# @work
export DOCKER_BETA=osxfs
export ROVER_AWS_FEDERATION_ROLE="arn:aws:iam::401027316804:role/FederatedProdSupportDeveloper"
#export ROVER_AWS_FEDERATION_ROLE="arn:aws:iam::401027316804:role/FederatedDeveloper"
ulimit -n 524288

source $HOME/projects/web/profile
source $HOME/.env