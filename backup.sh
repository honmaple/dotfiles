#!/bin/bash
vimrc="~/.vimrc"
bashrc="~/.bashrc"
xinitrc="~/.xinitrc"
conkyrc="~/.conkyrc"
xmodmap="~/.Xmodmap"
fonts="~/.fonts/"
themes="~/.themes/"
conkycolors="~/.conkycolors"
if [ ! -f "$bashrc" ]; then 
cp ~/.bashrc ./
fi 
if [ ! -f "$xinitrc" ]; then 
cp ~/.xinitrc ./
fi 
if [ ! -f "$conkyrc" ]; then 
cp ~/.conkyrc ./
fi 
if [ ! -f "$xmodmap" ]; then 
cp ~/.Xmodmap ./
fi 
if [ ! -d "$fonts"]; then 
cp -r ~/.fonts/ ./
fi 
if [ ! -d "$themes"]; then 
cp -r ~/.themes/ ./
fi 
if [ ! -d "$conkycolors"]; then 
cp -r ~/.conkycolors/ ./
fi 