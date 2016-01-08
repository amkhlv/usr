# Maildirs

For some strange reason, every maildir folder should contain a file named:

    ..maildir++

with the following content:

    maildir++ 1

Otherwise, `Evolution` will start renaming the folders in some very strange way (with lots of `5E` and `5F`).

# Setup

Notice that `localhost` appears to be invalid, need to use `127.0.0.1` instead (why???)

__Make sure that no thread view__

To use `sendmail` with `exim4`, say 

    server: 127.0.0.1
    port: 25
    cryptography: none

Also, goto `Edit` → `Preferenes` → `Mail preferences` and tick on empty trash on exit

# Troubleshooting

## If contact modification starts to misbehave

Try:

    pkill evolution
    rm -rf ~/.cache/evolution/

__ATTN__ :

1. It will take some time to start `evolution` next time, just to rebuild indices

2. It is better, before doing this, to delete all the search folders


