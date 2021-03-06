Registering new mime type
=========================

For example, I want to create a mime type for `.scrbl` files.

For local user
--------------

Create a file `/tmp/amkhlv-ejs.xml` containing:

    <?xml version="1.0"?>
    <mime-info xmlns='http://www.freedesktop.org/standards/shared-mime-info'>
      <mime-type type="text/x-ejs">
        <comment>Express templates</comment>
        <glob pattern="*.ejs"/>
      </mime-type>
    </mime-info>

Then say: 

    xdg-mime install amkhlv-ejs.xml
    update-mime-database ~/.local/share/mime/



Globally
--------

I create a file: `/usr/share/mime/packages/scribble.xml` containing these lines:

    <?xml version="1.0" encoding="UTF-8"?>
    <mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">
      <mime-type type="application/x-scribble">
        <comment>Racket Scribble File</comment>
        <glob pattern="*.scrbl"/>
      </mime-type>
    </mime-info>

and then run the following command:

    update-mime-database /usr/share/mime

(__takes a few minutes!__)

