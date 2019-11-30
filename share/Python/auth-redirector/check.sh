#!/bin/env bash

(
    source bin/activate
    cd redirector

    SECRET_KEY=a SOCIAL_AUTH_GOOGLE_OAUTH2_KEY=b SOCIAL_AUTH_GOOGLE_OAUTH2_SECRET=c ./manage.py check --deploy
)
