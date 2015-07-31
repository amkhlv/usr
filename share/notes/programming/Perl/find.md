General idea
============

    use File::Find;

    my $dir = # whatever you want the starting directory to be

    find(\&do_something_with_file, $dir);

    sub do_something_with_file
    {
        #.....
    }


Special variables
=================

The `do_something_with_file`  function takes no arguments but rather does its work through a collection of variables.

    $File::Find::dir is the current directory name,
    $_ is the current filename within that directory
    $File::Find::name is the complete pathname to the file.

You are chdir()'d to `$File::Find::dir` when the function `do_something_with_file` is called, unless `no_chdir` was specified.


find2perl
=========

There is a fantastic tool called `find2perl`. The syntax is like GNU `find` but instead of doing the search, it outputs the
translation to the corresponding Perl program.
