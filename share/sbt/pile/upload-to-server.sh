#!/bin/bash

rsync -cav --delete target/universal/stage/  saudade:/var/www/pile-app/

ssh saudade "chown -R www-data:www-data /var/www/pile-app/ "

