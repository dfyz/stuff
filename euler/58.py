from nzmath.prime import *
from nzmath.rational import Rational

pc = 0
c = 1

d = 1
cur = 1
last_cur = cur
while True:
  for _ in xrange(4):
    cur += d + 1
    if primeq(cur):
      pc += 1
    c += 1
  d += 2
  ratio = Rational(pc, c)
  if cur - last_cur > 10000:
      print cur, float(ratio.numerator) / ratio.denominator
      last_cur = cur
  if ratio < Rational(1, 10):
    print ratio, d
    break