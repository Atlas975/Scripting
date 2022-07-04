#!/bin/bash


newCode(){
    code $*
    clear
    echo -e "\e[0;34m$USER \033[0mcreated\e[1;32m $*\033[0m under \e[1;33m$(pwd | rev | cut -f1 -d'/' - | rev)\033[0m on\e[1;32m $(uname -sr  | cut -f1 -d'-')\033[0m"
}

testCode(){
   cd /home/adilw/Dropbox/Adil_Code/TempCode
   ncode temp.$*
}


alias tcode=testCode
alias ncode=newCode
