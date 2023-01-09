#!/bin/bash

export gbase="git@github.com:Atlas975"

git config --global alias.ad add
git config --global alias.st status
git config --global alias.ch checkout
git config --global alias.ro remote
git config --global alias.co commit
git config --global alias.br branch
git config --global alias.pl pull
git config --global alias.ps push
git config --global alias.cl clone
git config --global alias.in init
git config --global alias.rm remove
git config --global alias.lo log



# macro aliases
alias g-cam="git commit -am"
alias g-chb="git checkout -b"
alias g-org="git remote set-url origin"
alias g-mps="git add . && git commit -m ""$*"" && git push"
# alias g-aps="git add . && git commit -m ""$(TZ='UTC' date +'%T-%D (UTC)')"" && git push"




