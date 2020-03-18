# Isolating rules with NFTables

The goal is to run `google-chrome` in such a way that it can only
access a locally running `Express.js` server. 
(Which we use as a GUI for a `Node` program.)

Create new group `no-intenet`. 
Start `google-chrome` as follows:

    sg no-internet -c "google-chrome-stable --user-data-dir=/path/to/MyProfile"

Suppose that its `gid` is 1001. Add in `/etc/nftables.conf` , 
and that the local `Express.js` server is listening on port 11111. Then:

    table inet filter {
        ...
        chain output {
            ...
            skgid 1001 ip daddr 127.0.0.0/8 accept
            tcp dport 11111 counter drop
            skgid 1001 counter drop
            ...
        }
    }

(This means that only the processes run by the `no-interget` group can have access to
the `Express.js` server, but at the same time they do not have access to the internet.)
