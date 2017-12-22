How to include all keys and certificates inside the config file
===============================================================

Such config files usually have the extension `.ovpn`

They are like usual `.conf` files, but have certs at the end like this:

    key-direction 1
    
    <tls-auth>
    #
    # 2048 bit OpenVPN static key
    #
    -----BEGIN OpenVPN Static key V1-----
    f745... rest of tls key
    -----END OpenVPN Static key V1-----
    </tls-auth>
    
    <ca>
    -----BEGIN CERTIFICATE-----
    MIIG... rest of ca.crt data
    -----END CERTIFICATE-----
    </ca>
    
    <cert>
    -----BEGIN CERTIFICATE-----
    MIIH... rest of client/user .crt data
    -----END CERTIFICATE-----
    </cert>
    
    <key>
    -----BEGIN PRIVATE KEY-----
    MIIJ... rest of client/user .key data
    -----END PRIVATE KEY-----
    </key>


Notice that `key-direction` is needed; `key-direction 1` corresponds to `1` in `tls-auth ta.key 1`

