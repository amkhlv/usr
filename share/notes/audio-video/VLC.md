# VLC


## Streaming audio from microphone to public

Suppose that a computer is connected to a server over VPN. 

Notations:

1. `COMP_VPN_IP` --- the VPN address of the computer

2. `SERV_VPN_IP` --- the VPN address of the server

3. `SERV_IP` --- the public Internet address of the server

4. `COMP_VPN_PORT` and `SERV_PORT` --- the ports we use 


Execute on computer:

    vlc -vvv --clock-jitter=100 --sout-udp-caching=100  --sout-rtp-caching 100 pulse:// --sout '#transcode{acodec=ulaw,ab=128}:rtp{dst='${COMP_VPN_IP}',port='${COMP_VPN_PORT}',sdp=rtsp://'${COMP_VPN_IP}:${COMP_VPN_PORT}'/internal.sdp}'

Execute on server:

    cvlc --clock-jitter=100  --sout-udp-caching=100 --sout-rtp-caching=100 --network-caching=100 rtsp://${COMP_VPN_IP}:${COMP_VPN_PORT}/internal.sdp  --sout '#transcode{acodec=ulaw,ab=128}:rtp{dst='${SERV_IP}',port='${SERV_PORT}',sdp=rtsp://'${SERV_IP}:${SERV_PORT}'/mystream.sdp}'

Audience shouls execute:

    vlc --network-caching=100 rtsp://${SERV_IP}:${SERV_PORT}/mystream.sdp

### Firewall rules

We should open the following ports:

1. `COMP_VPN_PORT` (on the VPN interface of your computer) for both `TCP` and `UDP` 

2. `SERV_PORT` (on the server's public interface) for both `TCP` and `UDP`

3. `COMP_VPN_PORT + 1` and `SERV_PORT + 1` just for `UDP`.

### Comments

1. Low values of caching are to decrease latency.

2. The last command works on Debian, but does not work on Android.



