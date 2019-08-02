#!/bin/bash
# ********************************************************************************
# Copyright © 2018 jianglin
# File Name: pacman-disowned.sh
# Author: jianglin
# Email: xiyang0807@gmail.com
# Created: 2018-04-11 10:07:38 (CST)
# Last Update: Wednesday 2018-07-11 10:16:16 (CST)
#          By:
# Description:
# ********************************************************************************

tmp=${TMPDIR-/tmp}/pacman-disowned-$UID-$$
db=$tmp/db
fs=$tmp/fs

mkdir "$tmp"
trap  'rm -rf "$tmp"' EXIT

pacman -Qlq | sort -u > "$db"

find /bin /etc /lib /sbin /usr \
  ! -name lost+found \
  \( -type d -printf '%p/\n' -o -print \) | sort > "$fs"

comm -23 "$fs" "$db"
