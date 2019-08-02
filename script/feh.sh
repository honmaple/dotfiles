#!/bin/bash
# **************************************************************************
# Copyright © 2017 jianglin
# File Name: feh.sh
# Author: jianglin
# Email: lin.jiang@upai.com
# Created: 2017-12-10 23:51:01 (CST)
# Last Update: Friday 2019-08-02 21:06:45 (CST)
#          By:
# Description:
# **************************************************************************

shopt -s nullglob

if [[ ! -f $1 ]]; then
    echo "$0: first argument is not a file" >&2
    exit 1
fi

file=$(basename -- "$1")
dir=$(dirname -- "$1")
arr=()
shift

cd -- "$dir"

for i in *; do
    [[ -f $i ]] || continue
    arr+=("$i")
    [[ $i == $file ]] && c=$((${#arr[@]} - 1))
done

exec feh -d --scale-down -- "${arr[@]:c}" "${arr[@]:0:c}"
