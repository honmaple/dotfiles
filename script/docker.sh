#!/bin/bash
# ********************************************************************************
# Copyright © 2018 jianglin
# File Name: docker.sh
# Author: jianglin
# Email: xiyang0807@gmail.com
# Created: 2018-02-24 10:17:04 (CST)
# Last Update: 星期六 2018-02-24 10:21:21 (CST)
#          By:
# Description:
# ********************************************************************************
docker_build(){
    docker build --no-cache -t "$2" .
}

docker_run(){
    docker build --no-cache -t "$2" .
}

docker_into(){
    docker exec -it "$2" /bin/bash
}


case "$1" in
    build)
        docker_build
        ;;
    run)
        docker_run
        ;;
    into)
        docker_into
        ;;
    *)
        usage
        ;;
esac
