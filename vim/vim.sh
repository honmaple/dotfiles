#!/bin/bash
echo "你需要安装到的环境是(n/s)?"
echo "n for own,s for service"
read answer
if [ $answer = "s" ];then
    echo "服务"
    mv .vimrc_service ~/
elif [ $answer = "n" ];then
    echo "个人"
    mv .vimrc ~/
else
    echo "输入错误"
    exit 2
fi
echo "正在安装······^_^"
sudo pacman -S ctags the_silver_searcher gcc cmake
echo "正在clone vundle······"
git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
echo "正在安装插件······"
echo "请耐心等待"
vim -c "BundleInstall" -c "q" -c "q"
echo "插件安装完成······"
# echo "正在编译YouCompleteMe······"
# cd ~/.vim/bundle/YouCompleteMe/
# ./install.py --clang-completer
# echo "编译完成"
