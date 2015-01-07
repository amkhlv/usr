#!/bin/sh

[ $3 ] || { echo "USAGE: $0  cryptdevice  keydevice  target_name_such_eg_sda1_crypt" ; exit 1 ; }

fatal_error() {
    echo "Boot Error: $1" > /dev/console
    exit 1
}

cryptdevice=$1
keydevice=$2
sdxx_crypt=$3
hostname=`cat /etc/hostname`

# create mount point and mount the opened device
if [ ! -d /mnt/bootkey ]; then
	mkdir -p -m 0700 /mnt/bootkey > /dev/null 2>&1
fi
mount -r -t ext2 $keydevice /mnt/bootkey > /dev/null 2>&1

mount -obind /dev/console /dev/tty > /dev/null
echo "Hi!  I am your console..." > /dev/console
/lib/cryptsetup/askpass "PASS :" | gpg --homedir /mnt/bootkey/.gnupg --passphrase-fd 0 --decrypt /mnt/bootkey/${hostname}.gpg | cryptsetup --key-file=- luksOpen $cryptdevice $sdxx_crypt
umount /dev/tty

# unmount and close the USB Keys
umount /mnt/bootkey > /dev/null 2>&1
rmdir /mnt/bootkey > /dev/null 2>&1

mount -o ro /dev/mapper/$sdxx_crypt / 

[ $? = 0 ] && { echo "SUCCESS! Now type exit and press RETURN" ; } || { echo "There was some ERROR" ; }

