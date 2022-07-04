#!/bin/bash

updating(){
    echo -e "\e[0;33mChecking for package updates...\e[0m"
    sudo dnf update -y
    echo -e "\e[0;33mChecking for package upgrades...\e[0m"
    sudo dnf upgrade -y
}

cleaning(){
    echo -e "\e[0;31m";
    echo -e "Cleaning system...";
    sudo dnf autoremove -y;
    rm -rf /home/adilw/Dropbox/Adil_Code/TempCode/*;
    rm -rf ~/.local/share/Trash/*;
    rm -f ~/Downloads/*;
    rm -f ~/Pictures/*
}

alias fedup=updating
alias farbros="conda activate /home/adilw/anaconda3/envs/farbros";
alias orion="/home/adilw/Dropbox/Adil_Code/Orion; clear; farbros";
alias notes="/home/adilw/Dropbox/Adil_Notes; clear";
alias trim="conda deactivate;clear";
