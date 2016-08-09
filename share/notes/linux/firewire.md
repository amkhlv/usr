# Update

Excellent discussion can be found [here](http://security.stackexchange.com/questions/88629/preventing-dma-attacks)


# Finding out if firewire is present

## Using `lsmod`

    lsmod | grep 1394
    lsmod | grep fire

But I guess it will not show if there is no actual `firewire` port on the computer. Because then they will not be loaded, in
the first place. However, proceed to disabling them.

## \*\*\*ATTENTION\*\*\* `juju`

There is a \`\`new firewire stack'' called __juju__. It is not clear to me if it has the same module names.

# Disabling firewire

If you don't use Firewire at all, you can simply `rmmod ohci1394`, and (for a permanent fix) add the following lines in:

    gvim  /etc/modprobe.d/blacklist 

and then (important!) __run afterwords:__!

    update-initramfs -k all -u

Here are lines to add to `/etc/modprobe.d/blacklist`:

    # Prevent automatic loading of the ohci1394 module.
    blacklist ohci1394
    blacklist sbp2
    blacklist ieee1394
    blacklist raw1394
    blacklist eth1394
    blacklist dv1394
    blacklist firewire-ohci
    blacklist firewire-core
    blacklist firewire-sbp2
    # Prevent manual loading of the ohci1394 module.
    install ohci1394 false
    install sbp2 false
    install ieee1394 false
    install raw1394 false
    install eth1394 false
    install dv1394 false
    install firewire-ohci false
    install firewire-core false
    install firewire-sbp2 false
    # Iff we should ever load the ohci1394 module, force the use of the 'phys_dma=0' option.
    options ohci1394 phys_dma=0



__Update initramfs afterwards__:

    update-initramfs -k all -u

As for the new "juju" Firewire stack, I'm not so sure. A few quick tests showed that the currently available tools don't work with the new stack, but you shouldn't feel too secure! AFAIK the new stack does support (or will support soon) physical DMA for Firewire, so it's probably just a matter of adapting the tools a bit (I'll do some testing/research on this later, as time permits).

# To read

Interesting paper, including the research on the new `juju` stack, is [here](file://ieee-1394-forensics.pdf).

# Other interfaces

There is a very interesting paper [here](file://report-on-DMA-attacks.pdf). The table from that paper:

                 Protocol        Risk level

                 eSATA           Low
                 DisplayPort     None
                 FireWire        High
                 PCI             High
                 Pc Card         High
                 Thunderbolt     Medium
                 USB             None
                 USB OTG         High

    Table 2.1: Enumeration of interfaces and their risk level

# Possible solutions

> &#x3d;item\* disable PC Card and FireWire in BIOS
>
> or
>
> &#x3d;item\* use hybernation
>
> or
>
> &#x3d;item\* insert something into PC Card 
>
> that would turn off the computer when removed
