#!/bin/bash
gitTotal() {
    git add .
    git commit -m "$*"
    git push
}
gitAuto() {
    git add .
    git commit -m "Auto commit"
    git push
}

gitNew() {
    git clone "git@github.com:Atlas975/$*"
    cd $*
    git remote set-url origin https://github.com/Atlas975/$*
    clear
    echo -e "cloned and linked repository\e[1;32m $*"
}

alias pgit=gitPush
alias agit=gitAuto
alias ngit=gitNew
alias cgit="git clone $*"
