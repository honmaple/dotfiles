#!/bin/bash
#**************************************************************************
#Copyright © 2016 jianglin
#File Name: upgrade_pg.sh
#Author: jianglin
#Email: xiyang0807@gmail.com
#Created: 2016-11-20 15:19:18 (CST)
#Last Update: Friday 2019-08-02 21:09:37 (CST)
#          By:
#Description:
#**************************************************************************/
## Set the old version that we want to upgrade from.
export FROM_VERSION=10

# pacman -S --needed postgresql-old-upgrade
# chown postgres:postgres /var/lib/postgres/
# su - postgres -c "mv /var/lib/postgres/data /var/lib/postgres/data-${FROM_VERSION}"
su - postgres -c 'mkdir /var/lib/postgres/data'
su - postgres -c "initdb --locale en_US.UTF-8 -E UTF8 -D /var/lib/postgres/data"
su - postgres -c "pg_upgrade -b /opt/pgsql-${FROM_VERSION}/bin/ -B /usr/bin/ -d /var/lib/postgres/data-${FROM_VERSION} -D /var/lib/postgres/data"
