Anytype self-hosting
====================

    docker run -d \
      -e ANY_SYNC_BUNDLE_INIT_EXTERNAL_ADDRS="100.100.100.11" \
      -p 33010:33010 \
      -p 33020:33020/udp \
      -v /home/andrei/data:/data \
      --restart unless-stopped \
      ghcr.io/grishy/any-sync-bundle:1.4.2-2026-04-07


