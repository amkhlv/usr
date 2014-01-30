.. amkhlv documentation master file, created by
   sphinx-quickstart on Mon Feb 18 16:24:02 2013.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

amail
=====

.. automodule:: amail
   :members: command_line_arguments, emailfile

issues
======

Sometimes when I send email it gets saved in two places: `maildirs/sent-mail/` and `maildirs/gmail_sent-mail`
and the `From` field is different. This is **duplicate with different headers but same messageid**, leading to errors like 
*Something wrong, maybe duplicate email?* when executing `amail.py -m`

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

