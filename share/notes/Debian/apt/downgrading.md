# Downgrade from `unstable` to `testing`

In `/etc/apt/sources.list` put the `testing` lines [as I explain here](testing.md) 

Also, create the file `/etc/apt/preferences` and put the following lines in it:

    Package: *
    Pin: release a=testing
    Pin-Priority: 1001

Then do:

    apt-get update
    apt-get upgrade
    apt-get dist-upgrade



# Downgrade from `unstable` to `stable`

<div>
    <span style="color:reg;"><b> I had problems, maybe do not recommend! </b></span>
</div>
