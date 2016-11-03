#!/usr/bin/env python3

import argparse
import sys
import hashlib
import struct
import socket

parser = argparse.ArgumentParser(description='Send (client) or receive (server) a stream over a socket')
parser.add_argument('--host', dest ='host', default="localhost", help="host")
parser.add_argument('-p', '--port', dest='port', help='port number')
parser.add_argument('-s', '--server', dest='am_server', action="store_true", help="run as server (as opposed to client)")
parser.add_argument('--size', dest="chunk_size", default=str(16*1024*1024), help="size of chunk (default 16 Mb)")

args = parser.parse_args()

inbuff = sys.stdin.buffer

COMM = {'STOP': 0 , 'CONT' : 1, 'RSND' : 2, 'OK': 3}

def send_bytes(sock, bs):
    totalsent = 0
    MSGLEN = len(bs)
    while totalsent < MSGLEN:
        sent = sock.send(bs[totalsent:])
        if sent == 0:
            raise RuntimeError("socket connection broken")
        totalsent = totalsent + sent

def recv_bytes(sock, MSGLEN):
    chunks = []
    bytes_recd = 0
    while bytes_recd < MSGLEN:
        chunk = sock.recv(min(MSGLEN - bytes_recd, 2048))
        if chunk == '':
            sock.close()
            raise RuntimeError("socket connection broken")
        chunks.append(chunk)
        bytes_recd = bytes_recd + len(chunk)
    return b''.join(chunks)

def run_client(host,port):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((host, port))
    while True:
        bs = inbuff.read(int(args.chunk_size))
        if bs == b'' : 
            send_bytes(s, struct.pack('b', COMM['STOP']))
            s.close()
            print("end of stream")
            exit(0)
        else :
            send_bytes(s, struct.pack('b', COMM['CONT']))
            hash = hashlib.md5()
            hash.update(bs)
            digest = hash.digest()
            send_bytes(s, digest)
            send_bytes(s, struct.pack('l', len(bs)))
            reply = COMM['RSND']
            second_pass = False
            while reply != COMM['OK']:
                if second_pass: 
                    sys.stdout.write('x')
                else:
                    sys.stdout.write('.')
                sys.stdout.flush()
                second_pass = True
                send_bytes(s,bs)
                reply = struct.unpack('b', recv_bytes(s,1))[0]

def run_server(host,port):
    dsize = hashlib.md5().digest_size
    serversocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    serversocket.bind((host, port))
    serversocket.listen(5)
    #we only accept one client:
    (clientsocket, address) = serversocket.accept()
    while True:
        command = struct.unpack('b', recv_bytes(clientsocket, 1))[0]
        if command == COMM['STOP'] : 
            sys.stdout.buffer.flush()
            clientsocket.close()
            serversocket.close()
            exit(0)
        else :
            digest = recv_bytes(clientsocket, dsize)
            bslen  = recv_bytes(clientsocket, 8)
            reply = COMM['RSND']
            while reply != COMM['OK']:
                hash = hashlib.md5()
                bs = recv_bytes(clientsocket, struct.unpack('l', bslen)[0])
                hash.update(bs)
                if hash.digest() == digest : 
                    reply = COMM['OK']
                    sys.stdout.buffer.write(bs)
                send_bytes(clientsocket, struct.pack('b', reply))

if __name__ == '__main__' :
    if args.am_server : run_server(args.host,int(args.port))
    else : run_client(args.host,int(args.port))
