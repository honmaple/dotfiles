#!/bin/bash
#**************************************************************************
#Copyright © 2016 jianglin
#File Name: install.sh
#Author: jianglin
#Email: xiyang0807@gmail.com
#Created: 2016-06-21 14:38:53 (CST)
#Last Update: Monday 2018-11-19 14:57:42 (CST)
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
install emacs.d $HOME/.emacs.d
install vim $HOME/.vim
install i3 $HOME/.i3
install bashrc $HOME/.bashrc
install codeblocks $HOME/.codeblocks

# for location in $(find home -name '.*'); do
#   file="${location##*/}"
#   file="${file%.sh}"
#   link "$dotfiles/$location" "$HOME/$file"
# done
