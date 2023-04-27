#!/bin/bash
args=()
for arg in "$@"
do
    args+=("temp.$arg")
done

ncd -t "${args[@]}"
cd "/home/adilw/Dropbox/Adil_Code/Temp_Code" || exit
