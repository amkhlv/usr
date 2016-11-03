
Register new protocol
=====================

For example, if I want to open `tel:1234-5678` , I have to go to `about:config` and create new `boolean` variable:

    network.protocol-handler.expose.tel = false

Then, next time I click on `tel:...` link, I will be asked which program to choose. Sample program `tel-handler.sh`:

    #!/bin/bash

    NUMBER=${1#tel:}
    amkhlv_linksys.py -n "$NUMBER"

    

