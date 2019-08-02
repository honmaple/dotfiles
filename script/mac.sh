#!/bin/bash
#**************************************************************************
#Copyright © 2017 jianglin
#File Name: mac.sh
#Author: jianglin
#Email: lin.jiang@upai.com
#Created: 2017-09-05 17:47:43 (CST)
#Last Update: Friday 2019-08-02 21:08:57 (CST)
#          By:
#Description:
#**************************************************************************/
usage(){
    echo  "Usage:[e|w|enp7s0|wlp6s0]"
    exit 1
}

change_enp7s0(){
    sudo ip link set dev enp7s0 down
    sudo macchanger -enp7s0
    sudo ip link set dev enp7s0 up
}

change_wlp6s0(){
    sudo ip link set dev wlp6s0 down
    sudo macchanger -e wlp6s0
    sudo ip link set dev wlp6s0 up
}
case "$1" in
    w)
        change_wlp6s0
        ;;
    e)
        change_enp7s0
        ;;
    wlp6s0)
        change_wlp6s0
        ;;
    enp7s0)
        change_enp7s0
        ;;
    *)
        usage
        ;;
esac
