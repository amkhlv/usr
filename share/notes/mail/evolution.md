# Setup

__Make sure that no thread view__

To use `sendmail` with `exim4`, say 

    server: localhost
    port: 25
    cryptography: none

# Troubleshooting

If contact modification starts to misbehave, try:

    pkill evolution
    rm -rf ~/.cache/evolution/

__ATTN__ :

1. It will take some time to start `evolution` next time, just to rebuild indices

2. It is better, before doing this, to delete all the search folders


