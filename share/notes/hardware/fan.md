Controlling the fan
===================

First install the programs:

    aptitude install lm-sensors fancontrol

Then execute:

    sensors-detect

and answer YES to all YES/no questions

To configure the fan speed:

    pwmconfig

Finally:

    systemctl start fancontrol
    systemctl enable fancontrol


Monitoring the processor temperature
====================================

Execute:

    sensors

