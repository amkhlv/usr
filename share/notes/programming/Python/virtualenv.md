

Installing library
==================

    aptitude install python3-venv


Initializing
============

    python3 -m venv myproject


Using
=====

Sourcing
--------

Excellent guide is [available here](https://docs.python.org/3/tutorial/venv.html)

    cd myproject
    source bin/activate

Notice that after sourcing from `bin/activate`, the folder `/path/to/myproject/bin/` gets appended to `$PATH`.
In particular, we now have `pip`


Installing packages
-------------------


Freeze
------

    pip freeze > requirements.txt

Then, to __reproduce on a different virtualenv__ : 

    pip install -r requirements.txt


