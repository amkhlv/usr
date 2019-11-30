#!/bin/env bash

(
    source bin/activate
    pip install -r requirements.txt

    SECRET_KEY=a SOCIAL_AUTH_GOOGLE_OAUTH2_KEY=b SOCIAL_AUTH_GOOGLE_OAUTH2_SECRET=c redirector/manage.py migrate
)
