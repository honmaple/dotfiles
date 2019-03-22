#!/bin/bash
#**************************************************************************
#Copyright © 2016-2019 jianglin
#File Name: install.sh
#Author: jianglin
#Email: mail@honmaple.com
#Created: 2016-06-21 14:38:53 (CST)
#Last Update: Friday 2019-03-22 10:54:44 (CST)
#          By:
#Description:
#**************************************************************************/
# echo 'You might need to change your default shell to zsh: `chsh -s /bin/zsh` (or `sudo vim /etc/passwd`)'

# dir="$HOME/git"
# cd $dir
# git clone https://github.com/honmaple/dotfiles.git honmaple
# cd honmaple

# dotfiles="$HOME/git/honmaple"

# if [[ -d "$dotfiles" ]]; then
#   echo "Symlinking dotfiles from $dotfiles"
# else
#   echo "$dotfiles does not exist"
#   exit 1
# fi

install() {
    from="$1"
    to="$2"
    read -p "Linking '$from' to '$to' [Y/n/q]? "  answer
    answer="${answer:-Y}"

    if [ "$answer" == "Y" ] || [ "$answer" == "y" ];then
        echo "Linking '$from' to '$to'"
    elif [ "$answer" == "q" ];then
        exit 0
    fi
    # rm -f "$to"
    # ln -s "$from" "$to"
}
clean() {
    to="$1"
    read -p "Clean '$to' [Y/n/q]? "  answer
    answer="${answer:-Y}"

    if [ "$answer" == "Y" ] || [ "$answer" == "y" ];then
        echo "Clean '$to'"
    elif [ "$answer" == "q" ];then
        exit 0
    fi
}

install emacs.d $HOME/.emacs.d
install vim $HOME/.vim
install i3 $HOME/.i3
install bashrc $HOME/.bashrc
install codeblocks $HOME/.codeblocks
install fonts $HOME/.local/share/fonts

# for location in $(find home -name '.*'); do
#   file="${location##*/}"
#   file="${file%.sh}"
#   link "$dotfiles/$location" "$HOME/$file"
# done
