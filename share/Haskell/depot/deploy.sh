#!/bin/bash

[ "$1" ] || { echo first argument should be server name ; exit 1 ; }

ssh $1 "
  cd /var/www ;
  [ -d depot ] || { mkdir depot ; mkdir depot/uploads ; }
"

scp users.sqlite $1:/var/www/depot/users.sqlite.new

find .stack-work/dist/ -type f -name depot-exe -exec scp {} $1:/usr/local/bin/ \;

ssh $1 "
  chown -R www-data:www-data /var/www/depot ;
  cd /var/www/depot ; 
  [ -f users.sqlite ] && { rm users.sqlite.new ; } || { mv users.sqlite.new users.sqlite ; }
"

