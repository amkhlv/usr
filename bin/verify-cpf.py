#!/usr/bin/python

"""Example: verify-cpf.py 1 2 3 4 5 6 7 8 9"""

import sys

v0 = 0 ; v1 =0 ;

for j in range(0,9):
    print sys.argv[j+1]
    v0 = v0 + (10 - j) * int(sys.argv[j+1])
    v1 = v1 + (11 - j) * int(sys.argv[j+1])

v0 = ( 11 - v0 ) % 11
if v0 > 9 :
    v0 = 0

v1 = v1 + 2*v0
v1 = ( 11 - v1 ) % 11
if v1 > 9 :
    v1 = 0

print '-'
print v0
print v1


