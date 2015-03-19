#!/bin/bash

rsync -cav --delete target/universal/stage/  saudade:/var/www/chat-app/

ssh saudade "chown -R www-data:www-data /var/www/chat-app/ "

