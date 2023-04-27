#!/bin/bash


git_msg_push() {
    git add .
    git commit -m "$*"
    git push -u
}

git_auto_push() {
    git add .
    git commit -m "Auto commit $(TZ='UTC' date +'%T %d/%m/%y (UTC)')"
    git push -u
}

git_remote_personal() {
    git init
    git remote add origin git@github.com:Atlas975/"$*".git
    git remote set-url origin git@github.com:Atlas975/"$*".git
    git pull git@github.com:Atlas975/"$*".git main
    git add .; git commit -m "Initial commit"; git push -u origin main
}

git_finalize_feature() {
    git checkout main
    git pull
    git merge "$*" && git add . && git commit -m "Merged $*" && git push && git branch -d "$*"
}

git_ignore() {
    touch .gitignore; echo -e "$*" >> .gitignore
}

alias g-atls=git_remote_personal
alias g-mps=git_msg_push
alias g-aps=git_auto_push
alias g-fin=git_finalize_feature
alias g-ig=git_ignore
