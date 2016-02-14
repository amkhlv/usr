Using the host PulseAudio in container
======================================

On the host
-----------

### Adjustment to PulseAudio

Programs interact with PulseAudio via either `shm` or `UNIX socket`.
We need to configure it to use `UNIX socket`, but the default is `shm`.
So, we need to create a file:

    /home/andrei/.pulse/client.conf

containing the line:

    disable-shm=yes

After that, we should restart `PulseAudio`. We should now get available the UNIX socket `/run/user/1000/pulse/native`


### Parameters of systemd-nspawn

We need to add `--bind=/run/user/1000/pulse:/run/user/host/pulse` to the command line parameters of `systemd-nspawn`


In container
------------

Now, after we start the container, we will have available a UNIX socket:

    /run/user/host/pulse/native

We should tell the applications to use it. One way to do it is via an __environment variable__ :

    PULSE_SERVER=/run/user/host/pulse/native

