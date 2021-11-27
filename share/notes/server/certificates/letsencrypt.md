Links
=====

[Info from snel.com](https://www.snel.com/support/install-lets-encrypt-ssl-on-debian-9-running-nginx-web-server/)


Install certbot
===============

    aptitude install certbot

Temporary point webroot to the usual place
==========================================

Edit `/etc/nginx/sites-available/default` or such
(__the one which controls__ `http:` and __not__ `https:` !), and make sure:

    root /var/www/;

(That's where the `certbot` will put its `tmp` staff, like `.well-known/...` and such)

Run certbot
===========

    certbot certonly --webroot -w /var/www/ -d andreimikhailov.com

List the obtained certificates in NGINX config
==============================================

Edit `/etc/nginx/sites-available/ssl` or whatever, and make sure it has lines:

    ssl_certificate /etc/letsencrypt/live/andreimikhailov.com/fullchain.pem;
    ssl_certificate_key  /etc/letsencrypt/live/andreimikhailov.com/privkey.pem;

Setting up for the auto-renewal
===============================

They are __only valid for 90 days__ 

First [Temporary point webroot to /var/www](#temporary-point-webroot-to-the-usual-place)

Then:

    certbot renew
