
References
==========

[how-to-prepare-patches-for-debian-packages](http://raphaelhertzog.com/2011/07/04/how-to-prepare-patches-for-debian-packages/)

[how-to-use-quilt-to-manage-patches-in-debian-packages](http://raphaelhertzog.com/2012/08/08/how-to-use-quilt-to-manage-patches-in-debian-packages/)

[IntroDebianPackaging](https://wiki.debian.org/IntroDebianPackaging)


Prerequisites
=============

    aptitude install build-essential devscripts debhelper


Step-by-step
============

First of all make sure that you `~/.quiltrc` is set up as described [here](http://raphaelhertzog.com/2012/08/08/how-to-use-quilt-to-manage-patches-in-debian-packages/).

We use the `wordpress` package as an example. To check it out, do:

    debcheckout wordpress
    apt-get source wordpress

This will create two directories: `wordpress` (which is the `git` repository) and `wordpress-0.4.1`

`cd` to the root of the `git` repository (_i.e._ `cd wordpress`)

Then:

    dch --nmu

This simply modifies the `debian/changelog`

Now apply all already existing packages:

    quilt push -a

Create my own package:

    quilt new fix_foobar.patch

Now for every `src/file.c` which you want to edit, say:

    quilt add src/file.c

After editing, refresh the `quilt`:

    quilt refresh

and try to build:

    debuild -us -uc

To install the generated package, say:

    sudo debi wordpress_0.4.1-4.1_i386.changes

Finally, to send the patch to the actual developer, say:

    debdiff wordpress_0.4.1-4.dsc wordpress_0.4.1-4.1.dsc 


