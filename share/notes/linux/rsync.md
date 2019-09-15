Backup root filesystem
======================

    rsync -caAXv / --exclude={"/dev/*","/proc/*","/sys/*","/tmp/*","/run/*","/mnt/*","/media/*","/lost+found"}   /backup/

Here `-X` is to preserve extended attributes, and `-A` for `ACL`

