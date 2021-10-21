Pins
====

On USB2TTL adapter:

1. TX is green

2. RX is white

Connect three wires:

1. TX ↔ RX 

2. RX ↔ TX. 

3. Ground (black on USB2TTL) to ground.

__Do not connect power (the red one, or "V")__ Only three wires are connected.

Terminal
========

User should be in the `dialout` group.

Maybe this:

    screen /dev/ttyUSB0 115200,-parenb,-cstopb,cs8

Alternatives to screen
----------------------

    busybox microcom /dev/ttyUSB0 -s 115200

or:

    tio /dev/ttyUSB0


