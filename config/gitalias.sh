#!/bin/bash

git config --global alias.ad add
git config --global alias.st status
git config --global alias.me merge
git config --global alias.ch checkout
git config --global alias.ro remote
git config --global alias.co commit
git config --global alias.br branch
git config --global alias.pl pull
git config --global alias.ps push
git config --global alias.cl clone
git config --global alias.in init
git config --global alias.rm remove
git config --global alias.ss stash
git config --global alias.lo log



# macro aliases
git_msg_push() {
    git add .
    git commit -m "$*"
    git push
}

git_auto_push() {
    git add .
    git commit -m "Auto commit $(TZ='UTC' date +'%T-%D (UTC)')"
    git push
}

git_remote_personal() {
    git remote set-url origin git@github.com:Atlas975/"$*".git
}

git_revert_commit() {
    git revert "$*"
}

alias g-cam="git commit -am"
alias g-chb="git checkout -b"
alias g-org="git remote set-url origin"
alias g-atls=git_remote_personal
alias g-mps=git_msg_push
alias g-aps=git_auto_push
alias g-rev=git_revert_commit


