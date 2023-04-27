#!/bin/bash
echo -e "\e[0;31m";
echo -e "Cleaning system...";
sudo dnf autoremove -yq;
echo -e "Cleaning cargo cache...";
find Dropbox/Adil_Code/ -name "Cargo.toml" -type f -execdir cargo clean \;
echo -e "Cleaning trash...";
rm -rf ~/.local/share/Trash/*
rm -rf ~/Downloads/*
rm -rf ~/Pictures/*
echo -e "\e[0;32m";
echo -e "System cleaned!";

# bleachbit || exit
