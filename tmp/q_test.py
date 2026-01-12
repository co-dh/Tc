#!/usr/bin/env python3
# Quick test to see what's on the kdb server
import socket

def q(cmd):
    s = socket.socket()
    s.connect(('localhost', 8888))
    # kdb IPC: 1-byte endian, 1-byte async, 2-byte unused, 4-byte msg len, then data
    msg = cmd.encode() + b'\x00'
    hdr = bytes([1, 1, 0, 0]) + (8 + len(msg)).to_bytes(4, 'little')
    s.sendall(hdr + msg)
    resp = s.recv(4096)
    s.close()
    return resp

print(q("tables[]"))
print(q("count each tables[]"))
