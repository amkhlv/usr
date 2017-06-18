Running virt-viewer as ordinary user
====================================

    virt-viewer -c qemu:///system MyMachine

Use a floating window, because fullscreen leads to rescaling.


Mouse grab
==========

Using `virsh edit MyMachine` add the following node to the `<devices>` section:

    <input type='tablet' bus='usb'>
    </input>

(Because of some magic, `virsh` will automatically add the `<address ... />` line between `<input>` and `</input>`)

This will make mouse pointer not grabbed at all.
