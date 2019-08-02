#!/bin/bash
# ********************************************************************************
# Copyright © 2019 jianglin
# File Name: emacs.sh
# Author: jianglin
# Email: mail@honmaple.com
# Created: 2019-07-09 15:11:07 (CST)
# Last Update: Tuesday 2019-07-09 17:56:59 (CST)
#          By:
# Description:
# ********************************************************************************
EMACS=/usr/bin/emacs
GUI=0
ARGS="$@"
ARRAY=("-nw" "--no-window-system" "--batch" "--help")

for arg in $ARGS; do
    for keyword in "${ARRAY[@]}";do
        if [ $keyword = $arg ];then
            GUI=1
            break
        fi
    done
    if [ $GUI -eq 1 ]; then
        break
    fi
done

if [ $GUI -eq 1 ]; then
    $EMACS $ARGS
else
    $EMACS $ARGS &> /dev/null &
    # nohup $EMACS $ARGS &> /dev/null & disown
fi
