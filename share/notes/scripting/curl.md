Check if website is up
======================

    [ "$(curl --connect-timeout 10  -I  -s   example.com | head -n 1 | grep "200.*OK")" ]       && echo -n "y"     || echo -n "n" 

