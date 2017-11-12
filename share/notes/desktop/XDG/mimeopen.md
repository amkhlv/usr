WARNING
=======


__DO NOT USE__ `mimeopen` because it uses the wrong configuration file.

Use `xdg-open` instead.


Description
===========

`mimeopen` is a standalone part of `libfile-mimeinfo-perl`: "Perl module to determine file types".
This is a standalone program (written in Perl) which opens `URL` according to its `MIME` type.

Use
===

    mimeopen somefile.ext

If it does not know how to handle this `mime` type, then it will ask to choose from a list!

`mimeopen` stores the knowledge of `mime` type association in `~/.local/share/applications/defaults.list`, example:

    [Default Applications]
    application/pdf=pdq.desktop;
    text/markdown=emacs24.desktop;

__But the right file__ is `~/.local/share/applications/mimeapps.list` which has the same syntax,
