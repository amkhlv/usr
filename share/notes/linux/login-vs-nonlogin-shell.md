How to say which one
====================

Nice discussion on [Stack Exchange](http://unix.stackexchange.com/questions/38175/difference-between-login-shell-and-non-login-shell):
	

To tell if you are in a login shell:

    prompt> echo $0
    -bash # "-" is the first character. Therefore, this is a login shell.

    prompt> echo $0
    bash # "-" is NOT the first character. This is NOT a login shell.



What is read for config
=======================




When bash is invoked as an interactive login shell, or as a non-interactive shell with the `--login` option,
it first reads and executes commands from the file `/etc/profile`, if that file exists.

After reading that file, it looks for `~/.bash_profile`, `~/.bash_login`, and `~/.profile`,
in that order,
and reads and executes commands from __the first one that exists and is readable__.

The `--noprofile` option may be used when the shell is started to inhibit this behavior.
