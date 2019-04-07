DO NOT use targetcli
====================

__DO NOT USE__ `targetcli` or `targetcli-fb`

The configuration is actually very simple:

tgt
===

    aptitude install tgt

The __config file__ is `/etc/tgt/targets.conf` . It is very short:

    default-driver iscsi
    
    <target iqn....:firstdrivename>
      direct-store /dev/sda1
      initiator-name iqn....
      incominguser aaa passwordaaa
      outgoinguser bbb passwordbbb
    </target>

    <target iqn....:seconddrivename>
      direct-store /dev/sda2
      initiator-name iqn....
      incominguser aaa passwordaaa
      outgoinguser bbb passwordbbb
    </target>

where:

1. `initiator-name` should match the one provided in `/etc/iscsi/initiatorname.iscsi` on the client (called "initiator") machine

2. `incominguser aaa passwordaaa` should match the data in `/etc/iscsi/iscsid.conf` on the client:
    `node.session.auth.username` should be `aaa` and `node.session.auth.password` should be `passwordaaa`

3. `outgoinguser bbb passwordbbb` should match `node.session.auth.username_in` and `node.session.auth.password_in` in `/etc/iscsi/iscsid.conf`


Enabling
========

    systemctl enable tgt

