# NFTables

## Debugging

First, add `nftrace` to the rule. For example:

    tcp dport 10000 counter nftrace set 1 drop

Then, run command:

    nft monitor trace


