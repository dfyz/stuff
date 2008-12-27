from nzmath.prime import *
from itertools import *

def CoolPair(a, b):
  def Cat(a, b):
    return int(str(a) + str(b))
  return primeq(Cat(a, b)) and primeq(Cat(b, a))

MAX = 5000

def GenFrom(n):
  return takewhile(lambda x: x <= MAX, dropwhile(lambda x: x <= n, generator()))

for p1 in GenFrom(3):
  for p2 in GenFrom(7):
    for p3 in GenFrom(109):
      for p4 in GenFrom(673):
        for p5 in GenFrom(p4):
          ps = [ p1, p2, p3, p4, p5 ]
          ok = True
          for i in xrange(len(ps)):
            if not ok:
              break
            for j in xrange(i + 1, len(ps)):
              ok = ok and CoolPair(ps[i], ps[j])
              if not ok:
                break
          print ps, ok
          if ok:
            exit(0)