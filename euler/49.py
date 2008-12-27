from itertools import permutations
from nzmath.prime import *

def Num(arr):
  return arr[0] * 1000 + arr[1] * 100 + arr[2] * 10 + arr[3]

def Try(arr):
  perms = list(set(permutations(arr)))
  perms.sort()
  for i in xrange(len(perms)):
    for j in xrange(i + 1, len(perms)):
      for k in xrange(j + 1, len(perms)):
        a, b, c = Num(perms[i]), Num(perms[j]), Num(perms[k])
        if primeq(a) and primeq(b) and primeq(c) and b - a == c - b and 1000 <= a < b < c:
          print str(a) + str(b) + str(c)

for a in xrange(0, 10):
  for b in xrange(a, 10):
    for c in xrange(b, 10):
      for d in xrange(c, 10):
        Try([a, b, c, d])