Command line
============

should include the line:

    -monitor telnet:localhost:9312,server,nowait

Then, to connect to it:

    telnet localhost 9312

Send keys
=========

Such as:

    sendkey ctrl-u

    sendkey ctrl-alt-f1

Full list:

    meta_l  meta_r
    shift 	shift_r 	alt 	alt_r 	altgr 	altgr_r
    ctrl 	ctrl_r 	menu 	esc 	1 	2
    3 	4 	5 	6 	7 	8
    9 	0 	minus 	equal 	backspace 	tab
    q 	w 	e 	r 	t 	y
    u 	i 	o 	p 	ret 	a
    s 	d 	f 	g 	h 	j
    k 	l 	z 	x 	c 	v
    b 	n 	m 	comma 	dot 	slash
    asterisk 	spc 	caps_lock 	f1 	f2 	f3
    f4 	f5 	f6 	f7 	f8 	f9
    f10 	num_lock 	scroll_lock 	kp_divide 	kp_multiply 	kp_subtract
    kp_add 	kp_enter 	kp_decimal 	sysrq 	kp_0 	kp_1
    kp_2 	kp_3 	kp_4 	kp_5 	kp_6 	kp_7
    kp_8 	kp_9 	< 	f11 	f12 	print
    home 	pgup 	pgdn 	end 	left 	up
    down 	right 	insert 	delete

The Win keys are `meta_l` and `meta_r`

Screen and audio grabs
======================

screendump

    screendump filename

Capture a screendump and save into a PPM image file.

    wavcapture filename

Capture the sound of the vm and save it into a specified .wav file.

    stopcapture index

Stop recording the wavcapture. Index of the first wavcapture is 0. 

Virtual machine control
=======================

    quit or q

Quit QEMU immediately.

    savevm name

Save the virtual machine as the tag 'name'. Not all filesystems support this. raw does not, but qcow2 does.

    loadvm name

Load the virtual machine tagged 'name'. This can also be done on the command line: -loadvm name

    stop

Suspend execution of VM

    cont

Reverse a previous stop command - resume execution of VM.

    system_powerdown

This has an effect similar to the physical power button on a modern PC. The VM will get an ACPI shutdown request and usually shutdown cleanly.

Devices
=======

    change device setting

The change command allows you to change removable media (like CD-ROMs), change the display options for a VNC, and change the password used on a VNC.

    (qemu) info block
    ide0-hd0: type=hd removable=0 file=/path/to/winxp.img
    ide0-hd1: type=hd removable=0 file=/path/to/pagefile.raw
    ide1-hd1: type=hd removable=0 file=/path/to/testing_data.img

    eject [-f] device

Use the eject command to release the device or file connected to the removable media device specified. The -f parameter can be used to force it if it initially refuses!


