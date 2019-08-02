#!/bin/bash
#**************************************************************************
#Copyright © 2017 jianglin
#File Name: xrandr.sh
#Author: jianglin
#Email: lin.jiang@upai.com
#Created: 2017-12-11 17:24:19 (CST)
#Last Update:星期四 2017-12-28 16:55:53 (CST)
#          By:
#Description:
#**************************************************************************/
# eDP1  自带显示器
# HDMI1 扩展显示器

# 显示器独立输出到右边
# xrandr --output HDMI1 --auto --output eDP1 --auto --right-of HDMI1
# 关闭显示器
# xrandr --output HDMI1 --off
# 同步显示
# xrandr --output HDMI1 --auto

# 图形界面
# arandr
output_on(){
    xrandr --output HDMI1 --auto --output eDP1 --auto --right-of HDMI1
}

output_off(){
    xrandr --output HDMI1 --off
}

output_same(){
    xrandr --output HDMI1 --auto
}

case "$1" in
    on)
        output_on
        ;;
    off)
        output_off
        ;;
    same)
        output_same
        ;;
    *)
        usage
        ;;
esac
