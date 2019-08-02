#!/bin/bash
# ********************************************************************************
# Copyright © 2018 jianglin
# File Name: port.sh
# Author: jianglin
# Email: xiyang0807@gmail.com
# Created: 2018-03-12 11:59:37 (CST)
# Last Update: Monday 2018-05-14 11:52:50 (CST)
#          By:
# Description:
# ********************************************************************************
# ps auxf | grep `netstat -apn | grep 8000 `
netstat -apn | grep 8000
ss -lptn 'sport = :80'
ss -lptn | grep 8000
