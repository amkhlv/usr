Registering a user
==================

    /nick somename
    /msg nickserv register password email

Registering a channel
=====================

Registration
------------

    /nick somename
    /join #example
    /msg chanserv register #example somepassord somedescription

Turning on secret mode
----------------------

At this point `somename` is the __founder__ of the channel `#example`.


__NOT SURE ABOUT THIS:__ At the top right corner of the `xchat` window there are buttons. Press `SECRET` and `RESTRICTED` buttons.


Alternatively:

     /msg chanserv set secure #example on
     /msg chanserv set restricted #example on

Then add another user to the access list:

     /msg chanserv access #example add otherusername 10

where `10` is the __level__ ; to get information about levels say:

     /msg chanserv levels #example list

